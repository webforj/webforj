package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.builder.MarkdownBuilder;
import com.webforj.devtools.craftforj.docs.fetcher.DwcFetcher;
import com.webforj.devtools.craftforj.docs.index.DocsIndex;
import com.webforj.devtools.craftforj.docs.model.DocsEntry;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;

/**
 * Resolver for documentation from the pre-built docs-index.json.
 *
 * <p>
 * Looks up the component in the docs index and combines the pre-built content with DWC styling data
 * fetched at runtime.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class IndexDocsResolver implements DocsResolver {

  private final DocsIndex docsIndex;
  private final DwcFetcher dwcFetcher;

  /**
   * Creates a new IndexDocsResolver with the given dependencies.
   *
   * @param docsIndex the docs index
   * @param dwcFetcher the DWC fetcher
   */
  public IndexDocsResolver(DocsIndex docsIndex, DwcFetcher dwcFetcher) {
    this.docsIndex = docsIndex;
    this.dwcFetcher = dwcFetcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(DocsQuery query) {
    // Try server component name first
    DocsEntry entry = docsIndex.findByServerComponent(query.getServerComponent());
    if (entry == null) {
      // Try client component name
      entry = docsIndex.findByClientComponent(query.getClientComponent());
    }

    if (entry == null) {
      return null;
    }

    // Fetch DWC styling if component has a client component tag
    DwcStylingData styling = null;
    String clientTag = entry.getClientComponent();
    if (clientTag == null) {
      clientTag = query.getClientComponent();
    }
    if (clientTag != null && !clientTag.isEmpty()) {
      styling = dwcFetcher.fetch(clientTag);
    }

    // Build and return markdown
    return MarkdownBuilder.build(entry, styling);
  }
}
