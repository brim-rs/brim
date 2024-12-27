#define TRY(expr) ({                                              \
    auto&& _try_tmp_##__LINE__ = expr;                           \
    if (!_try_tmp_##__LINE__.has_value())                        \
        return std::unexpected(_try_tmp_##__LINE__.error());     \
    _try_tmp_##__LINE__.value();                                 \
})