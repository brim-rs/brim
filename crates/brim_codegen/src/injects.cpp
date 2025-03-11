#if (defined(__GNUG__) || defined(__clang__)) && !defined(_WIN32)
#include <cxxabi.h>
#endif

inline void panic_impl(const std::string& message,
                       const std::source_location& location) {
    std::cerr << "\n[PANIC] " << message << "\n    at " << location.file_name()
              << ":" << location.line() << ":" << location.column() << " in "
              << location.function_name() << std::endl;
    std::abort();
}

#define PANIC(message) panic_impl(message, std::source_location::current())

template <typename... Args>
inline void panic_format_impl(const std::string& format,
                              const std::source_location& location,
                              Args&&... args) {
    std::ostringstream message;

    size_t pos = 0;
    size_t arg_index = 0;
    const auto print_arg = [&message, &arg_index, &args...](auto process_arg) {
        if constexpr (sizeof...(args) > 0) {
            ((arg_index == 0 ? (message << args, true) : false) || ...);
            arg_index++;
        }
    };

    while (pos < format.size()) {
        size_t open_brace = format.find('{', pos);
        if (open_brace == std::string::npos) {
            message << format.substr(pos);
            break;
        }

        message << format.substr(pos, open_brace - pos);

        if (open_brace + 1 < format.size() && format[open_brace + 1] == '}') {
            print_arg([](auto arg) { return arg; });
            pos = open_brace + 2;
        } else {
            message << '{';
            pos = open_brace + 1;
        }
    }

    panic_impl(message.str(), location);
}

#define PANIC_F(format, ...) \
    panic_format_impl(format, std::source_location::current(), ##__VA_ARGS__)

inline std::string demangle(const char* name) {
#if (defined(__GNUG__) || defined(__clang__)) && !defined(_WIN32)
    int status = -1;
    std::unique_ptr<char, void (*)(void*)> res{
        abi::__cxa_demangle(name, nullptr, nullptr, &status), std::free};
    return (status == 0) ? res.get() : name;
#else
    return name;
#endif
}

class TypeInfo {
   private:
    std::type_index type_idx;
    std::string demangled_name;
    bool is_const;
    bool is_reference;
    bool is_pointer;
    bool is_rvalue_ref;

   public:
    template <typename T>
    static TypeInfo create() {
        const std::type_info& raw_type = typeid(T);
        std::string name = demangle(raw_type.name());

        bool is_const = std::is_const_v<std::remove_reference_t<T>>;
        bool is_reference = std::is_lvalue_reference_v<T>;
        bool is_pointer = std::is_pointer_v<std::remove_reference_t<T>>;
        bool is_rvalue_ref = std::is_rvalue_reference_v<T>;

        return TypeInfo(std::type_index(raw_type), std::move(name), is_const,
                        is_reference, is_pointer, is_rvalue_ref);
    }

    TypeInfo(std::type_index idx, std::string name, bool is_c, bool is_ref,
             bool is_ptr, bool is_rval_ref)
        : type_idx(idx),
          demangled_name(std::move(name)),
          is_const(is_c),
          is_reference(is_ref),
          is_pointer(is_ptr),
          is_rvalue_ref(is_rval_ref) {}

    const std::type_index& index() const { return type_idx; }
    const std::string& name() const { return demangled_name; }

    std::string full_description() const {
        std::ostringstream oss;
        if (is_const) oss << "const ";
        oss << demangled_name;
        if (is_pointer) oss << "*";
        if (is_reference) oss << "&";
        if (is_rvalue_ref) oss << "&&";
        return oss.str();
    }

    bool operator==(const TypeInfo& other) const {
        return type_idx == other.type_idx;
    }

    bool operator!=(const TypeInfo& other) const {
        return type_idx != other.type_idx;
    }
};

template <typename Base, typename... Args>
struct Visitor {
    virtual ~Visitor() = default;
    virtual void visit(Base&, Args...) = 0;
};

template <typename T, typename Base, typename... Args>
struct TypedVisitor : Visitor<Base, Args...> {
    std::function<void(T&, Args...)> callback;

    TypedVisitor(std::function<void(T&, Args...)> cb)
        : callback(std::move(cb)) {}

    void visit(Base& obj, Args... args) override {
        callback(static_cast<T&>(obj), std::forward<Args>(args)...);
    }
};

class SafeAny {
   private:
    std::any value;
    TypeInfo type_info;

    struct VoidType {};

   public:
    SafeAny()
        : value(),
          type_info(TypeInfo::create<VoidType>()) {}

    template <typename T, typename = std::enable_if_t<
                              !std::is_same_v<std::decay_t<T>, SafeAny>>>
    SafeAny(T&& val)
        : value(std::forward<T>(val)),
          type_info(TypeInfo::create<std::decay_t<T>>()) {}

    SafeAny(const SafeAny& other)
        : value(other.value),
          type_info(other.type_info) {}

    SafeAny(SafeAny&& other) noexcept
        : value(std::move(other.value)),
          type_info(other.type_info) {}

    template <typename T, typename = std::enable_if_t<
                              !std::is_same_v<std::decay_t<T>, SafeAny>>>
    SafeAny& operator=(T&& val) {
        value = std::forward<T>(val);
        type_info = TypeInfo::create<std::decay_t<T>>();
        return *this;
    }

    SafeAny& operator=(const SafeAny& other) {
        if (this != &other) {
            value = other.value;
            type_info = other.type_info;
        }
        return *this;
    }

    SafeAny& operator=(SafeAny&& other) noexcept {
        if (this != &other) {
            value = std::move(other.value);
            type_info = other.type_info;
        }
        return *this;
    }

    template <typename T, typename U>
    static SafeAny with_type(U&& val) {
        SafeAny result;
        result.value = std::any(std::forward<U>(val));
        result.type_info = TypeInfo::create<T>();
        return result;
    }

    void reset() {
        value.reset();
        type_info = TypeInfo::create<VoidType>();
    }

    bool has_value() const { return value.has_value(); }

    const TypeInfo& get_type_info() const { return type_info; }

    const std::type_index& type() const { return type_info.index(); }

    std::string get_type_name() const { return type_info.name(); }

    std::string get_full_type_description() const {
        return type_info.full_description();
    }

    template <typename T>
    T cast() const {
        using DecayedT = std::decay_t<T>;
        auto requested_type = TypeInfo::create<DecayedT>();

        if (!value.has_value()) {
            PANIC_F("Attempted to cast an empty SafeAny to {}",
                    requested_type.full_description());
        }

        if (type_info.index() != requested_type.index()) {
            PANIC_F(
                "Type mismatch in SafeAny::cast\n"
                "  Stored type: {}\n"
                "  Requested type: {}",
                type_info.full_description(),
                requested_type.full_description());
        }

        return std::any_cast<DecayedT>(value);
    }

    template <typename T>
    bool is() const {
        return value.has_value() &&
               (type_info.index() ==
                TypeInfo::create<std::decay_t<T>>().index());
    }

    template <typename T>
    std::optional<T> try_cast() const {
        using DecayedT = std::decay_t<T>;

        if (!value.has_value() ||
            type_info.index() != TypeInfo::create<DecayedT>().index()) {
            return std::nullopt;
        }

        return std::make_optional(std::any_cast<DecayedT>(value));
    }

    template <typename T>
    T* try_cast_ptr() {
        using DecayedT = std::decay_t<T>;

        if (!value.has_value() ||
            type_info.index() != TypeInfo::create<DecayedT>().index()) {
            return nullptr;
        }

        return std::any_cast<DecayedT>(&value);
    }

    template <typename T>
    const T* try_cast_ptr() const {
        using DecayedT = std::decay_t<T>;

        if (!value.has_value() ||
            type_info.index() != TypeInfo::create<DecayedT>().index()) {
            return nullptr;
        }

        return std::any_cast<DecayedT>(&value);
    }

    template <typename Base, typename... Args>
    bool visit(std::vector<std::unique_ptr<Visitor<Base, Args...>>>& visitors,
               Args... args) {
        if (!has_value()) return false;

        for (auto& visitor : visitors) {
            if (visitor) {
                visitor->visit(*std::any_cast<Base>(&value),
                               std::forward<Args>(args)...);
                return true;
            }
        }

        return false;
    }

    template <typename T, typename Base, typename... Args>
    static std::unique_ptr<Visitor<Base, Args...>> make_visitor(
        std::function<void(T&, Args...)> callback) {
        return std::make_unique<TypedVisitor<T, Base, Args...>>(
            std::move(callback));
    }

    void swap(SafeAny& other) noexcept {
        if (this != &other) {
            std::swap(value, other.value);
            std::swap(type_info, other.type_info);
        }
    }

    template <typename Tag, typename Value>
    static SafeAny with_tag(Value&& value) {
        struct TaggedValue {
            std::decay_t<Value> data;
            using tag_type = Tag;

            TaggedValue(Value&& v) : data(std::forward<Value>(v)) {}
        };

        return SafeAny(TaggedValue(std::forward<Value>(value)));
    }

    template <typename Tag>
    bool has_tag() const {
        return is<TaggedValue<Tag, std::any>>();
    }

    template <typename Tag>
    auto& get_tagged() {
        using TaggedType = TaggedValue<Tag, std::any>;

        if (!has_tag<Tag>()) {
            PANIC_F("SafeAny does not contain a value with tag {}",
                    demangle(typeid(Tag).name()));
        }

        return cast<TaggedType>().data;
    }

    template <typename Tag, typename T>
    struct TaggedValue {
        T data;
        using tag_type = Tag;

        TaggedValue(T&& v) : data(std::forward<T>(v)) {}
    };

    void debug_print(std::ostream& os = std::cerr) const {
        os << "SafeAny [" << (has_value() ? "has value" : "empty") << "]"
           << std::endl;
        os << "  Type: " << get_full_type_description() << std::endl;
    }

    friend std::ostream& operator<<(std::ostream& os, const SafeAny& any) {
        os << "SafeAny<" << any.get_type_name() << ">";
        return os;
    }
};

namespace std {
    template <>
    inline void swap(SafeAny& lhs, SafeAny& rhs) noexcept {
        lhs.swap(rhs);
    }
}