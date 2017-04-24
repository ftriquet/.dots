syntax match mytypes '\<t_[a-z_0-9]*\>'
highlight link mytypes cType

syntax match func '\(\<[a-z0-9_]*\>\)\((\)\@='
highlight link func Exception
