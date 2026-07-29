package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.fetcher.DwcFetcher;
import com.webforj.devtools.craftforj.docs.index.DocsIndex;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import java.util.Arrays;
import java.util.List;

/**
 * Main resolver that chains multiple resolvers in priority order.
 *
 * <p>
 * Resolution order:
 * <ol>
 * <li>Custom docs (META-INF/docs/custom/)</li>
 * <li>Pre-built docs index (docs-index.json)</li>
 * <li>DWC styling only (for dwc-* components)</li>
 * <li>HTML elements (for standard HTML tags)</li>
 * </ol>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ChainedDocsResolver implements DocsResolver {

  private final List<DocsResolver> resolvers;

  /**
   * Creates a new chained resolver with the default resolution order.
   */
  public ChainedDocsResolver() {
    DocsIndex docsIndex = new DocsIndex();
    DwcFetcher dwcFetcher = new DwcFetcher();

    this.resolvers =
        Arrays.asList(new CustomDocsResolver(), new IndexDocsResolver(docsIndex, dwcFetcher),
            new DwcOnlyResolver(dwcFetcher), new HtmlElementResolver());
  }

  /**
   * Creates a new chained resolver with custom resolvers.
   *
   * @param resolvers the resolvers to chain
   */
  public ChainedDocsResolver(List<DocsResolver> resolvers) {
    this.resolvers = resolvers;
  }

  @Override
  public String resolve(DocsQuery query) {
    for (DocsResolver resolver : resolvers) {
      String result = resolver.resolve(query);
      if (result != null) {
        return result;
      }
    }
    return null;
  }
}
