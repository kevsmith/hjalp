Installation

1. make
2. mkdir ~/.apps
3. mkdir ~/.ebin
4. cp -R hjalp ~/.apps/hjalp
5. cp examples/user_default.erl ~/.ebin
6. pushd . && cd ~/.ebin && erlc user_default.erl && popd
7. cp examples/dot_erlang ~/.erlang

To hjalp-enable your code add '-include("hjalp/include/hjalp.hrl")' to your source.
hjalp will take care of the rest!

Example hjalp session: https://gist.github.com/3973703

NOTE: hjalp is extremely dodgy proof-of-concept quality code. Guaranteed
to explode on unexpected inputs, set your code on fire, and abduct your cat.

Also, pull requests gleefully accepted.

YOU HAVE BEEN WARNED.

hjalp is published under the Apache 2.0 License.