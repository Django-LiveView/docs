# Website and documentation

This repository contains the source code for the website and documentation of Django Liveview.

It is a static website generated with [one.el](https://one.tonyaldon.com/).

## Do you want to contribute?

Edit `one.org`. It is a org file that contains the documentation and text of the website.

## Do you want to build the website?

Install Emacs in your system.

After that, you must install the `one.el` package.

```bash
M-x package-install RET one.el RET
```

Then, open `one.el` and run the following command:

```bash
M-x one-build
```

The website will be generated in the `public` directory.
