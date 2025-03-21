inline void panic_impl(const std::string& message,
                       const std::source_location& location) {
    std::cerr << "\n[PANIC] " << message << "\n    at " << location.file_name()
              << ":" << location.line() << ":" << location.column() << std::endl;
    std::abort();
}

#define PANIC(message) panic_impl(message, std::source_location::current())


template <typename... Args>
inline void panic_format_impl(const std::string& format,
                              const std::source_location& location,
                              Args&&... args) {
    std::ostringstream message;

    std::tuple<Args...> arg_tuple(std::forward<Args>(args)...);
    size_t arg_index = 0;

    size_t pos = 0;
    while (pos < format.size()) {
        size_t open_brace = format.find('{', pos);
        if (open_brace == std::string::npos) {
            message << format.substr(pos);
            break;
        }

        message << format.substr(pos, open_brace - pos);

        if (open_brace + 1 < format.size() && format[open_brace + 1] == '}') {
            if (arg_index < sizeof...(Args)) {
                size_t current_index = arg_index;
                ((current_index == 0 ?
                  (message << args, true) :
                  (--current_index == 0 ? (message << args, true) : false)) || ...);
            }
            arg_index++;
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


template <typename T>
T safe_cast(const std::any& value) {
    try {
        return std::any_cast<T>(value);
    } catch (const std::bad_any_cast&) {
        PANIC_F("Failed to cast std::any to requested type: {}. Found type: {}",
                typeid(T).name(), value.type().name());
    }
}