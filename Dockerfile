# FROM ocaml/opam:alpine-3.19-ocaml-5.1 AS init-opam
# RUN sudo apk update && sudo apk upgrade && 
# # && sudo apk add z3 \
# # && sudo apk add libstdc++ \
# # && sudo apk add bc


# FROM init-opam AS ocaml-base
# COPY . .
# RUN sudo apk add bash bash-doc bash-completion \
#     && opam install dune.3.14.2 menhir.20231231 utop \
#     && eval $(opam env) \
#     && dune build

# Use an official OCaml image
FROM ocaml/opam:debian-ocaml-4.14

# Set the working directory in the container
WORKDIR /app

# Copy the project files into the Docker container
COPY . /app

# Update the p  ackage list and upgrade the packages
RUN sudo apt-get update && sudo apt-get upgrade -y

# Install system dependencies for OCaml and Dune
RUN sudo apt-get install -y m4 libgmp-dev

# Install OCaml dependencies
RUN opam install dune ounit2 menhir

# Build the project using Dune
RUN eval $(opam env) && dune build && dune exec bin/main.exe
