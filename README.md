# __OCaml intro: thunks, streams, lazy evaluation and monads__

This is a lecture using OCaml intended to demonstrate building up basic
lists, then using that along with the notion of thunks to build basic
streams, followed by using the intrinsic lazy type as a memoizing 
alternative. There is also consideration of basic applicative and 
functional programming ideas and brief treatment of monads.  

A peripheral notion here is to using helpful tools like 
_vim-slime_ or similar alongside _tmux_, to execute the code step 
by step from _vim_ in _utop_ to show the concepts.  First 
install _vim_ and _tmux_.  To install _vim-slime_, first install 
_pathogen_, then install _vim_slime_ as a _pathogen_ bundle and 
add this to your _.vimrc_:

```
    execute pathogen#infect()                                       
    let g:slime_target = "tmux"                             
    let g:slime_paste_file = "$HOME/.slime_paste"
    let g:slime_default_config = {"socket_name": "default", "target_pane": "0.1"}
    let g:slime_dont_ask_default = 1
```

You can start the twin vertical view using the included _vv_ 
script as such:

```
    ./vv src/code.ml
```

Use C-c C-c to send code paragraphs from _vim_ to _utop_.  You can remap
the keys too, as with most any _vim_ commands.

[vim](http://www.vim.org/)

[tmux](https://tmux.github.io/)

[pathogen](https://github.com/tpope/vim-pathogen)

[vim-slime](https://github.com/jpalardy/vim-slime)