/**
 * Techblog OPML Generator
 * 
 * This script scrapes tech blog information from https://hatena.blog/dev/companies
 * and generates an OPML file for RSS readers.
 */

// The script is designed to be executed in browser console after scrolling to the bottom of the page

/**
 * Extract blog information from the hatena.blog/dev/companies page
 * @returns Array of [title, url] pairs
 */
function extractBlogInfo(): [string, string][] {
  const blogs: [string, string][] = [];
  
  // Get all company blog elements
  document.querySelectorAll('ul > li > div > div').forEach(element => {
    try {
      const titleElement = element.querySelector('a > h4');
      const linkElement = element.querySelector('a');
      
      if (titleElement && linkElement) {
        const title = titleElement.textContent?.trim() || '';
        const url = linkElement.getAttribute('href') || '';
        
        if (title && url) {
          // Assuming we need to add '/feed' to the URL to get the RSS feed
          const feedUrl = url.endsWith('/') ? `${url}feed` : `${url}/feed`;
          blogs.push([title, feedUrl]);
        }
      }
    } catch (error) {
      console.error('Error processing element:', error);
    }
  });
  
  return blogs;
}

/**
 * Generate OPML XML from blog information
 * @param blogs Array of [title, url] pairs
 * @returns OPML XML string
 */
function generateOPML(blogs: [string, string][]): string {
  const outlines = blogs.map(([title, url]) => 
    `    <outline text="${escapeXml(title)}" title="${escapeXml(title)}" type="rss" xmlUrl="${escapeXml(url)}"/>`
  ).join('\n');
  
  return `<?xml version="1.0" encoding="UTF-8"?>
<opml version="1.0">
  <head>
    <title>企業テックブログOPML</title>
  </head>
  <body>
${outlines}
  </body>
</opml>`;
}

/**
 * Escape XML special characters
 * @param text Text to escape
 * @returns Escaped text
 */
function escapeXml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&apos;');
}

/**
 * Main function to extract blogs and generate OPML
 * This should be run in the browser console after scrolling to the bottom of the page
 */
function main() {
  // NOTE: Make sure the page is fully scrolled before running this
  console.log("Extracting blog information...");
  const blogs = extractBlogInfo();
  console.log(`Found ${blogs.length} blogs`);
  
  const opml = generateOPML(blogs);
  console.log("OPML generated:");
  console.log(opml);
  
  // You can copy this to clipboard or download it
  // This part can only work in browser
  if (typeof navigator !== 'undefined' && navigator.clipboard) {
    navigator.clipboard.writeText(opml)
      .then(() => console.log("OPML copied to clipboard!"))
      .catch(err => console.error("Failed to copy OPML:", err));
  }
}

// If running in a Node.js environment, we can't directly scrape the page
// This is just for TypeScript compilation
if (typeof window === 'undefined') {
  console.log(`
  This script is designed to be run in a browser console.
  
  Instructions:
  1. Visit https://hatena.blog/dev/companies
  2. Scroll to the bottom of the page to load all companies
  3. Open the browser console (F12 or Ctrl+Shift+I)
  4. Copy and paste this entire script
  5. Run the main() function
  
  The OPML will be generated and displayed in the console output.
  `);
}

// To use directly in browser console, just call main()
// main();