package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.model.DocsQuery;

/**
 * Interface for resolving component documentation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface DocsResolver {

  /**
   * Attempts to resolve documentation for the given query.
   *
   * @param query the query containing component identifiers
   * @return the markdown documentation, or null if not resolved
   */
  String resolve(DocsQuery query);
}
