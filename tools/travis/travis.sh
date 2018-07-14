function linux_before_install()
{
    wget -O- -c https://www.dropbox.com/s/9besynm60tb3ao8/acats.tar.gz|tar xzf -
    cp -r tools/travis /tmp/
    cd ..
    tar --exclude=.git \
        -c -z -f /tmp/travis/gela.tar.gz gela
    docker build --tag gela /tmp/travis/
}

function linux_script()
{
    docker run -i -t --user=max gela /bin/bash -c \
           'tar xzf /src/gela.tar.gz -C ~ && make all check -C ~/gela '

}

${TRAVIS_OS_NAME}_$1
