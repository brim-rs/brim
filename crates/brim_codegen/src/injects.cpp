inline void panic(const std::string& message) {
    std::cerr << "[PANIC] " << message << std::endl;
    std::exit(1);
}

#define PANIC(msg) panic(msg)

template <typename... Args>
inline void panicf(const std::format_string<Args...> fmt, Args&&... args) {
    panic(std::format(fmt, std::forward<Args>(args)...));
}

#define PANIC_F(...) panicf(__VA_ARGS__)

template <typename T>
T unwrap(const std::optional<T>& opt) {
    if (!opt.has_value()) {
        PANIC_F("Attempted to unwrap an empty optional of type {}", typeid(T).name());
    }
    return *opt;
}

template<typename T, typename E>
T unwrap(const std::expected<T, E> &result) {
    if (!result.has_value()) {
        if constexpr (std::is_same_v<E, std::string>) {
            panic("Attempted to unwrap an error result: " + result.error());
        }
        else if constexpr (std::is_arithmetic_v<E>) {
            panic("Attempted to unwrap an error result: " + std::to_string(result.error()));
        }
        else {
            panic("Attempted to unwrap an error result (error details unavailable)");
        }
    }
    return *result;
}

#define TRY_IN_EXPR(expr) ([&]() -> decltype(expr) { \
    auto __res = (expr);                             \
    if (!__res) return std::unexpected(__res.error()); \
    return __res;                                    \
}())
