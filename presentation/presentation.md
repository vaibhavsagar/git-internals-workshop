% Historical Revisionism
% Vaibhav Sagar

# Working on a `git` library

<div class="notes">
So I've been at the Recurse Center for the last 8 weeks, mostly working on
implementing the conceptual core of `git` in Haskell as a way of learning
both Haskell and `git`.
</div>

# Sharing what I learned

<div class="notes">
I learned a lot about how repositories are implemented under the hood that I
thought it would be useful to know. I prepared a 5 minute presentation that
didn't go over well, but I had a lot of success with people one-on-one walking
them through a `.git` folder. I wanted something more scalable and interactive
so I decided to prepare a workshop.
</div>

# Preparing a workshop

<div class="notes">
I knew I wanted slides/notes as well, sample
`git`-compatible code, and a `git` repository to play with, so then I thought
of putting the slides in the commit messages and having the repo be the
workshop
</div>

# Manually constructing a history

<div class="notes">
I thought about the work involved in making this repository: cherry-picking,
rebasing, editing. What if I make a mistake somewhere and have to do a lot
of tedious manual work all over again? I spun my wheels for a while.
</div>

# Remembering I have a `git` library

<div class="notes">
Hang on, don't I already have a way to make the history that I want?
</div>

# Writing code to make my repository

https://github.com/vaibhavsagar/git-internals-workshop

<div class="notes">
So I wrote some code that takes the directories I want in each revision and
links them together as a valid `git` repository!
</div>

# Demo

<a href="https://asciinema.org/a/88784" target="_blank"><img src="https://asciinema.org/a/88784.png" height=225 width=400/></a>

# You can write code to make any history
