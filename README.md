Ejabberd mod_log_rest
=====================

Author: Brendon Crawford <brendon@last.vc>
Homepage: https://github.com/last/ejabberd_mod_log_rest


About
-----

mod_log_rest is a ejabberd module aimed at logging chat messages
to a REST service via HTTP POST.
mod_log_rest was originally based on mod_log_chat
from Process One.


Requirements
------------

This module has only been tested on and is primarely intended to
run on Ubuntu 10.04 Server. It also requires Erlang and Ejabberd
to be installed from source, and will not work with the Ubuntu
Erlang and Ejabberd packages.


Installation
------------

    ###
    ##
    ## Erlang
    ##
    ###

    apt-get purge erlang
    apt-get build-dep erlang
    apt-get install autoconf
    apt-get install curl
    apt-get install libc6-dev
    apt-get install libncurses5-dev
    apt-get install m4
    apt-get install libssl-dev
    apt-get install openjdk-6-jdk
    cd /usr/local/src/
    git clone --recursive git://github.com/erlang/otp.git erlang-otp
    cd erlang-otp
    git checkout OTP_R15B
    ./otp_build autoconf
    ./configure --prefix=/usr/local
    make
    make install
    curl https://raw.github.com/agner/agner/master/scripts/oneliner | sh
    export ERL_LIBS="/usr/local/agner/packages"
    echo >> /etc/environment
    echo 'ERL_LIBS="/usr/local/agner/packages"' >> /etc/environment
    
    
    ###
    ##
    ## Ejabberd
    ##
    ###

    apt-get purge ejabberd*
    apt-get install imagemagick gs
    mkdir /usr/local/src/ejabberd-build/
    cd /usr/local/src/ejabberd-build/
    git clone --recursive git://github.com/processone/ejabberd.git
    git checkout v2.1.10
    cd src/
    ./configure --prefix=/usr/local
    make
    make install
    useradd -M -r -s /bin/sh -d /usr/local/var/lib/ejabberd ejabberd
    mkdir /usr/local/var/run/ejabberd
    chown -R ejabberd:ejabberd \
          /usr/local/var/lib/ejabberd \
          /usr/local/var/run/ejabberd \
          /usr/local/var/lock/ejabberdctl \
          /usr/local/etc/ejabberd
    chmod -R 700 /usr/local/var/lib/ejabberd /usr/local/var/run/ejabberd /usr/local/var/lock/ejabberdctl
    chmod -R 775 /usr/local/sbin/ejabberdctl /usr/local/etc/ejabberd
    chmod -R 775 /usr/local/lib/ejabberd/
    cd /usr/local/src/ejabberd-build/
    git clone --recursive git://github.com/last/ejabberd-init-ubuntu.git
    cp -f ejabberd-init-ubuntu/ejabberd /etc/init.d/ejabberd
    chmod +x /etc/init.d/ejabberd
    #
    # If you want, you can start ejabberd in the foreground with:
    #
    #   sudo service ejabberd live
    #
    # If you want to start ejabberd in the background, you would run:
    #
    #   sudo service ejabberd start
    #
    # If you want to have it run automatically in the
    # background, when you boot you will need to run the following command:
    #
    #   update-rc.d -f ejabberd defaults
    #

    
    ###
    ##
    ## Ejabberd Modules
    ##
    ###
    
    cd /usr/local/src/ejabberd-build/
    git clone --recursive git://github.com/last/ejabberd-modules.git
    #
    # Build mod_admin_extra
    #
    cd /usr/local/src/ejabberd-build/ejabberd-modules/mod_admin_extra/trunk/
    ./build.sh
    cp -f ./ebin/mod_admin_extra.beam /usr/local/lib/ejabberd/ebin/
    #
    # Build mod_rest
    #
    cd /usr/local/src/ejabberd-build/ejabberd-modules/mod_rest/trunk/
    ./build.sh
    cp -f ./ebin/mod_rest.beam /usr/local/lib/ejabberd/ebin/
    #
    # Build mod_log_rest
    # NOTE: Currently this is causing Ejabberd to crash,
    #
    agner install mochiweb
    agner install ibrowse
    cd /usr/local/src/ejabberd-build/
    git clone --recursive git://github.com/last/ejabberd_mod_log_rest.git
    cd ejabberd_mod_log_rest
    make
    cp -f ./ebin/mod_log_rest.beam /usr/local/lib/ejabberd/ebin/
    #
    # After installing, you will now need to add the proper configuration
    # options to the ejabberd config file. For more information on this,
    # please see "conf/ejabberd.cfg".
    # 

