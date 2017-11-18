#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# fun aliases
alias ls='ls --color=auto'
alias ll='ls -al'

# change PS1
PS1='\[\033[00;35m\]\u\[\033[0m\]@\[\033[00;36m\]\h\[\033[0m\]: \[\033[01;36m\]\w\[\033[01;37m\]\n$ \[\033[00m\]'
export PATH=/home/sarai/.npm-global/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:/usr/local/spark/bin
export SPARK_HOME=/usr/local/spark
export PYTHONPATH=/usr/local/spark/python/lib/py4j-0.10.4-src.zip:/usr/local/spark/python/lib/pyspark.zip:/usr/local/spark/python
alias emacs="emacs -nw"
alias ll="ls -al"
