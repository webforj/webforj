package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.builder.MarkdownBuilder;
import com.webforj.devtools.craftforj.docs.fetcher.DwcFetcher;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;

/**
 * Resolver for DWC components that have no documentation but do have styling data.
 *
 * <p>
 * Fetches DWC styling data and builds a markdown document containing only styling information.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DwcOnlyResolver implements DocsResolver {

  private final DwcFetcher dwcFetcher;

  /**
   * Creates a new DwcOnlyResolver with the given DWC fetcher.
   *
   * @param dwcFetcher the DWC fetcher
   */
  public DwcOnlyResolver(DwcFetcher dwcFetcher) {
    this.dwcFetcher = dwcFetcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(DocsQuery query) {
    String clientTag = query.getClientComponent();
    if (clientTag == null || clientTag.isEmpty()) {
      return null;
    }

    // Only resolve dwc-* components
    if (!clientTag.startsWith("dwc-")) {
      return null;
    }

    DwcStylingData styling = dwcFetcher.fetch(clientTag);
    if (styling == null || !styling.hasData()) {
      return null;
    }

    return MarkdownBuilder.buildStylingOnly(clientTag, styling);
  }
}
