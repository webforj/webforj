package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.builder.MarkdownBuilder;
import com.webforj.devtools.craftforj.docs.index.HtmlElements;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;

/**
 * Resolver for standard HTML elements.
 *
 * <p>
 * Checks if the client component is a standard HTML element and returns a template with an MDN
 * link.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HtmlElementResolver implements DocsResolver {

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(DocsQuery query) {
    String clientTag = query.getClientComponent();
    if (clientTag == null || clientTag.isEmpty()) {
      return null;
    }

    if (!HtmlElements.isHtmlElement(clientTag)) {
      return null;
    }

    return MarkdownBuilder.buildHtmlElement(clientTag);
  }
}
