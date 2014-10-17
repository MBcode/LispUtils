" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2002 Sep 19
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc
set et sw=4 ts=4 

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file
endif
"set ai
set history=60		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set ignorecase		"new
set smartcase		"newer
set lisp		"new
set sm			"new

set scrolloff=3
set shortmess=atI
set visualbell

" Low priority filename suffixes for filename completion {{{
set suffixes-=.h        " Don't give .h low priority
set suffixes+=.aux
set suffixes+=.log
set wildignore+=*.dvi
set wildignore+=*.a,*.o,*.fasl
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png
set wildignore+=.DS_Store,.git,.hg,.svn
set wildignore+=*~,*.swp,*.tmp
set suffixes+=.bak
set suffixes+=~
set suffixes+=.swp
set suffixes+=.o
set suffixes+=.class
" }}} 
 
" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" This is an alternative that also works in block mode, but the deleted
" text is lost and it only works for putting the current register.
"vnoremap p "_dp

"execute pathogen#infect() 

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on  "maybe always have on?
  set hlsearch
endif

syntax on  "try it

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  augroup END
   
" Programming settings {{{
    augroup prog
        au!
        au BufRead *.c,*.cc,*.cpp,*.h,*.java,*.json set formatoptions=croql cindent nowrap nofoldenable
        au BufEnter *.java      map <C-Return> :w\|:!javac %<CR>
        au BufEnter *.scala      map <C-Return> :w\|:!javac %<CR>
        au BufEnter *.c         map <C-Return> :w\|:!gcc %<CR>
        au BufEnter *.cc,*.cpp  map <C-Return> :w\|:!g++ %<CR>
        au BufLeave *.java,*.scala,*.json,*.c,*.cc unmap <C-Return>

        " Don't expand tabs to spaces in Makefiles
        au BufEnter  [Mm]akefile*  set noet
        au BufLeave  [Mm]akefile*  set et

        " Set up folding for python
        au FileType python set nofoldenable foldmethod=indent
    augroup END
    " }}}

  "for C-like programming, have automatic indentation:
  autocmd FileType slang set cindent tabstop=4 shiftwidth=4

  "Java Programms (Prof. Dr. Harald want exactly three blanks ... ;-)
  autocmd FileType java set tabstop=3 shiftwidth=3
  autocmd FileType java set makeprg="ant compile\ %"
  autocmd FileType java set errorformat=\"%f\"\\\,\ line\ %l.%c:%m\,\ %f:%l:%m

  "slrn is my newsreader
  autocmd BufRead .followup,.article,.letter set fo=tcq comments=n:>,n::,n:»,n:]

  "for actual C programming where comments have explicit end
  "characters, if starting a new line in the middle of a comment automatically
  "insert the comment leader characters:
  autocmd FileType c,cpp set formatoptions+=ro dictionary=$HOME/.vim/c_dictionary
                       \ tabstop=4 shiftwidth=4 noexpandtab cindent comments=sl:/*,mb:**,elx:*/

  "for Perl programming, have things in braces indenting themselves:
  autocmd FileType perl set smartindent tabstop=3 shiftwidth=3

  "for CSS, also have things in braces indented:
  autocmd FileType css set smartindent

  "for HTML, generally format text, but if a long line has been created leave it
  "alone when editing:
  "autocmd FileType html set formatoptions+=tl
  augroup xhtml
      au!
      autocmd BufRead  *html source $HOME/.vim/html.vim
      autocmd BufWrite *html   ks|call LastMod()|'s
  augroup END

  " indent xml code
  augroup xml
      map ,mf !xmllint --format --recover - 2>/dev/null<CR>
  "    au!
  "    autocmd BufWrite *xml exe ":silent 1,$!xmllint --format --recover - 2>/dev/null"
  augroup END

  "for both CSS and HTML, use genuine tab characters for indentation, to make
  "files a few bytes smaller:
  autocmd FileType html,css set noexpandtab tabstop=2

  "in makefiles, don't expand tabs to spaces, since actual tab characters are
  "needed, and have indentation at 8 chars to be sure that all indents are tabs
  "(despite the mappings later):
  autocmd FileType make     set noexpandtab shiftwidth=8
  autocmd FileType automake set noexpandtab shiftwidth=8 
 

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")
"nmap <Leader>lisp :source ~/.vim/lisp/vilisp/VIlisp.vim<CR>

set nrformats=hex

au BufRead *.asd set ft=lisp
au BufRead *.pprj set ft=lisp
au BufRead *.pins set ft=lisp
au BufRead *.pont set ft=lisp
au BufRead *.km set ft=lisp
au BufRead *.kb set ft=lisp
au BufRead *.kif set ft=lisp
au BufRead *.mops set ft=lisp
au BufRead *.ste set ft=lisp
au BufRead *.clj set ft=lisp
au BufRead *.cljs set ft=lisp
au BufRead *.plm set ft=lisp
au BufRead *.alg set ft=lisp
au BufRead *.art set ft=lisp
au BufRead *.vdf set ft=lisp
au BufRead [jfmasond][0-9]*[a-gl0-9_=-] set ft=lisp
au BufRead *.go set ft=c
"let g:nekthuth_sbcl = "/usr/local/bin/sbcl" 

au BufNewFile,BufRead *.clj setf clojure
au BufNewFile,BufRead *.cljs setf clojure

au BufRead *.js set ft=java
au BufRead *.json set ft=java
au BufRead *.ipynb set ft=java
au BufRead *.coffee set ft=java
au BufRead *.scala set ft=java

function Send_to_Screen(text)
  if !exists("g:screen_sessionname") || !exists("g:screen_windowname")
    call Screen_Vars()
  end

  echo system("screen -S " . g:screen_sessionname . " -p " . g:screen_windowname . " -X stuff '" . substitute(a:text, "'", "'\\\\''", 'g') . "'")
endfunction

function Screen_Session_Names(A,L,P)
  return system("screen -ls | awk '/Attached/ {print $1}'")
endfunction

function Screen_Vars()
  if !exists("g:screen_sessionname") || !exists("g:screen_windowname")
    let g:screen_sessionname = ""
    let g:screen_windowname = "0"
  end

  let g:screen_sessionname = input("session name: ", "", "custom,Screen_Session_Names")
  let g:screen_windowname = input("window name: ", g:screen_windowname)
endfunction 

vmap <C-c><C-c> "ry :call Send_to_Screen(@r)<CR>
nmap <C-c><C-c> vip<C-c><C-c>

nmap <C-c>v :call Screen_Vars()<CR> 

"set rtp+=~/.vim/bundle/vundle/
"call vundle#rc()

" let Vundle manage Vundle
" required!
"Bundle 'gmarik/vundle' 

"set relativenumber 
"set number  
"let g:slimv_impl = 'sbcl'
"let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"sbcl --load ~/.vim/slime/start-swank.lisp\""' 
"let g:slimv_swank_cmd = '! xterm -e sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &' 
"let g:slimv_swank_cmd = '! rlwrap sbcl --load /usr/share/common-lisp/source/slime/start-swank.lisp &' 
