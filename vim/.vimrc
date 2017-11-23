set nocompatible

set guifont=Iosevka\ 11
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guicursor="a:ver35-Cursor"
filetype off
filetype plugin indent on
set smartindent
syntax on

set tabstop=4
set shiftwidth=4
set noexpandtab
set number
" set cursorline
set termguicolors
set scrolloff=3

autocmd FileType javascript set tabstop=2 softtabstop=0 expandtab shiftwidth=2 nosmarttab
autocmd FileType ruby set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab
autocmd FileType lua set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab
autocmd FileType python set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab
autocmd FileType eruby set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab
autocmd FileType yaml set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab
autocmd FileType haskell set tabstop=4 softtabstop=4 expandtab shiftwidth=4 nosmarttab shiftround
autocmd FileType typescript set tabstop=2 softtabstop=2 expandtab shiftwidth=2 nosmarttab shiftround

set hidden

set mouse=

"Real-world encoding
set encoding=utf-8
set termencoding=utf-8

"Interpret modelines in files
set modelines=1

"Do not abandon buffers
set hidden

"More useful backspace behavior
set backspace=indent,eol,start

"Better search
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

"" Plugins
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" The prophet
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-rails'

" Snippets
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

" Misc
Plugin 'justinmk/vim-syntax-extra'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'majutsushi/tagbar'
Plugin 'Valloric/YouCompleteMe'
Plugin 'jiangmiao/auto-pairs'
Plugin 'guns/vim-sexp'
Plugin 'w0rp/ale'
Plugin 'godlygeek/tabular'

" Languages
""" Typescript
Plugin 'HerringtonDarkholme/yats.vim'
Plugin 'Quramy/tsuquyomi'
Plugin 'leafgarland/typescript-vim'
Plugin 'Shougo/vimproc'
"" Haskell
Plugin 'itchyny/vim-haskell-indent'
"" Rust
Plugin 'racer-rust/vim-racer'
Plugin 'rust-lang/rust.vim'
"" Go
Plugin 'fatih/vim-go'
"" JS
Plugin 'pangloss/vim-javascript'

" Themes
Plugin 'mhartington/oceanic-next'
Plugin 'KeitaNakamura/neodark.vim'
Plugin 'jacoborus/tender.vim'
Plugin 'ajh17/Spacegray.vim'
Plugin 'cocopon/iceberg.vim'
Plugin 'beigebrucewayne/hacked_ayu.vim'
Plugin 'hzchirs/vim-material'


nnoremap <C-t> :TagbarToggle<CR>

call vundle#end()

let g:javascript_plugin_flow = 1

function! <SID>SynStack()
	if !exists("*synstack")
		return
	endif
	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
nmap <C-S-O> :call <SID>SynStack()<CR>


set noswapfile
let mapleader=" "

set listchars=eol:¬,tab:>\ ,trail:~,extends:>,precedes:<
set encoding=utf-8

""MAPINGS
inoremap kj <Esc>

nnoremap <leader>j :bn<CR>
nnoremap <leader>k :bp<CR>
nnoremap <Tab> :tabnext<CR>
nnoremap <S-Tab> :tabprevious<CR>
nnoremap <C-l> <C-w>l
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j

nnoremap <leader>w :w<CR>
nnoremap <leader>, :sh<CR>
nnoremap <leader>; mmA;<Esc>`m
nnoremap <leader>g mmMmngg=G`nzz`m
nnoremap <leader>n :noh<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>x :x<CR>
nnoremap !! :!
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

vnoremap <leader>t :Tabularize /

" Treat wrapped lines like normal lines
noremap j gj
noremap k gk

nnoremap gj ddp
nnoremap gk ddkP
nnoremap go o<Esc>k

cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Qa qa
cnoreabbrev wQ wq
cnoreabbrev Wq wq
cnoreabbrev WQ wq
cnoreabbrev Q q
cnoreabbrev Qa! qa!

augroup templates
	autocmd BufNewFile *.sh 0r ~/.vim/templates/skeleton.sh
	autocmd BufNewFile *.py 0r ~/.vim/templates/skeleton.py
	" autocmd BufNewFile *.rb 0r ~/.vim/templates/skeleton.rb
augroup END

augroup filetypedetect
	au BufRead,BufNewFile,BufWinEnter *.ts set filetype=typescript
	au BufWritePost *.ts TsuGeterr
augroup END


set background=dark

" Theme
let g:gruvbox_bold=1
let g:gruvbox_italic=1
let g:gruvbox_contrast_dark = 'hard'
let g:alduin_Shout_Contract_Vampirism=1
let g:alduin_Shout_Animal_Allegiance = 1
colorscheme gruvbox

" hi Normal ctermbg=NONE

""Trailing whitespaces
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/

autocmd BufWritePre * :%s/\s\+$//e
set viminfo='100,\"2500,:200,%,n~/.viminfo

"""GO
" use goimports for formatting
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
let g:go_fmt_autosave = 1


set laststatus=2
if has("statusline")
	set statusline =          " clear
	set statusline+=%02n      " leading zero 2 digit buffer number
	set statusline+=\ %-0.30f      " file tail
	set statusline+=%r        " read only flag '[RO]'
	set statusline+=%m        " modified flag '[+]' if modifiable
	set statusline+=%h        " help flag '[Help]'
	set statusline+=%=        " left/right separation point
	set statusline+=\ %c      " column number
	set statusline+=:%l/%L    " line/lines
	set statusline+=\ %p%%    " percent of file
	set statusline+=%{&paste?'=':'\ '}
	set statusline+=%{&wrap?'<':'>'}
	set statusline+=%{ALEGetStatusLine()}
endif

let g:multi_cursor_next_key='<C-c>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-<tab>>"
let g:UltiSnipsEditSplit="vertical"

let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']


let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

if !exists("g:ycm_semantic_triggers")
  let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers['typescript'] = ['.']

let g:ycm_rust_src_path = '/home/francois/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'

let g:tsuquyomi_disable_quickfix = 1

let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_save = 1
let g:ale_set_quickfix = 1
let g:ale_open_list = 1
let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
let g:ale_linters = { 'rust': ['cargo'], 'python': ['flake8', 'mypy'], 'c': ['gcc'] }
let g:ale_rust_cargo_use_check = 1
let g:ale_python_mypy_options = '--ignore-missing-imports'
let g:ale_c_gcc_options = '-I includes -I libft/includes -I ../libft/includes -I ../common/includes'

" Auto close quickfix buffer when it's the last open buffer
aug QFClose
  au!
  au WinEnter * if winnr('$') == 1 && getbufvar(winbufnr(winnr()), "&buftype") == "quickfix"|q|endif
aug END

" set notermguicolors
" hi Normal ctermbg=NONE
