# Django LiveView Website and Documentation

This repository contains the source code for the website and documentation of Django LiveView v2.0.0.

It is a static website generated with [one.el](https://one.tonyaldon.com/).

## Contributing

Edit `one.org` - an Org mode file that contains all the documentation and website content.

## Building the Website

### Using Docker (Recommended)

```bash
docker compose up one-el
```

The website will be generated in the `public` directory.

### Local Build

1. Install Emacs in your system
2. Install the `one.el` package:
   ```
   M-x package-install RET one.el RET
   ```
3. Open `one.org` in Emacs
4. Run: `M-x one-build`

The website will be generated in the `public` directory.

## Serving Locally

```bash
docker compose up nginx
```

Then visit http://localhost:9100
