# Tech Blog OPML Generator

This project extracts tech blog information from [Hatena Blog's companies page](https://hatena.blog/dev/companies) and generates an OPML file for RSS readers.

## Scripts

The project includes two main scripts:

1. **tech-blog-opml.ts**: Browser console script to run directly in the browser after manually scrolling the page.
2. **scrape-and-generate.ts**: Automated script using Puppeteer to scroll the page and extract data.

## Setup and Usage

### Prerequisites

- Node.js (v14+)
- npm or yarn

### Installation

1. Clone or download this repository
2. Install dependencies:

```bash
npm install
# or
yarn install
```

### Running the automated script

This will launch a browser, scroll through the Hatena Blog companies page, and generate the OPML file:

```bash
npm start
# or
yarn start
```

The script will generate:

- `hatena-tech-blogs.opml` - OPML file for import into RSS readers
- `tech-blogs-list.ts` - TypeScript file with the extracted blog data

### Browser console method

If you prefer manual extraction:

1. Open [https://hatena.blog/dev/companies](https://hatena.blog/dev/companies) in your browser
2. Scroll to the bottom of the page to load all content
3. Open your browser's developer console (F12 or Ctrl+Shift+I)
4. Copy the contents of `tech-blog-opml.ts` and paste it into the console
5. Run `main()` in the console
6. The OPML will be generated and displayed in the console

## OPML Format

The generated OPML file follows standard format for RSS readers:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<opml version="1.0">
  <head>
    <title>企業テックブログOPML</title>
  </head>
  <body>
    <outline text="Company Name" title="Company Name" type="rss" xmlUrl="https://example.com/feed"/>
    <!-- More blogs... -->
  </body>
</opml>
```

## Importing the OPML

The generated OPML file can be imported into any RSS reader that supports OPML import, including:

- Feedly
- Inoreader
- Tiny Tiny RSS
- NewsBlur
- Feeder
- And many more

## License

This project is open-source software.