## traceroute

This is a MirageOS unikernel which conducts a traceroute to the given host.
[Read the blog post.](https://hannes.nqsb.io/Posts/Traceroute)

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.08.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.0.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/roburio/tlstunnel.git
$ mirage configure -t <your-favourite-target>
$ make depend
$ mirage build
```

## Installing as binary

Reproducible binaries are available at https://builds.robur.coop/job/traceroute/

## Questions?

Please open an issue if you have questions, feature requests, or comments.
