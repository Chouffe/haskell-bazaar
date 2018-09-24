FROM ubuntu:18.10

RUN \
  apt-get -y update && \
  apt-get upgrade -y && \
  apt-get -y install curl && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  apt-get -y purge --auto-remove curl

RUN \
  useradd --user-group --create-home user && \
  mkdir -p /home/user/app

COPY . /home/user/app/

RUN \
  chown -R user:user /home/user

USER user:user

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

WORKDIR /home/user/app

RUN \
  stack setup && \
  stack build && \
  stack build hindent hlint && \
  stack build haskell-bazaar:haskell-bazaar-test
