ebank
=====

An OTP application

Build
-----

    $ rebar3 compile

Build Docker Image
-----

    $ docker build -t my_erl_builder:latest . 

Run Docker Image
-----

    $ docker run -it my_erl_builder:latest [command]