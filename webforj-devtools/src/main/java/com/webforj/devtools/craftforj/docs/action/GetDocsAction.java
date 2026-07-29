package com.webforj.devtools.craftforj.docs.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import com.webforj.devtools.craftforj.docs.resolver.ChainedDocsResolver;
import com.webforj.devtools.craftforj.docs.resolver.DocsResolver;

/**
 * Action handler for fetching component documentation.
 *
 * <p>
 * Handles the "docs.get" action by resolving documentation for the requested component.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetDocsAction implements CraftforjActionHandler<GetDocsAction.Response> {

  /** The action name. */
  public static final String ACTION = "docs.get";

  private static final Gson GSON = new Gson();

  private final DocsResolver resolver;

  /**
   * Creates a new GetDocsAction with the default resolver chain.
   */
  public GetDocsAction() {
    this(new ChainedDocsResolver());
  }

  /**
   * Creates a new GetDocsAction with a custom resolver.
   *
   * @param resolver the resolver to use
   */
  public GetDocsAction(DocsResolver resolver) {
    this.resolver = resolver;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Response handle(JsonObject params) {
    DocsQuery query = GSON.fromJson(params, DocsQuery.class);
    if (query == null) {
      query = new DocsQuery(null, null);
    }

    String markdown = resolver.resolve(query);

    return new Response(markdown);
  }

  /**
   * Response containing component documentation.
   */
  public static class Response {

    private final String markdown;

    /**
     * Creates a new docs response.
     *
     * @param markdown the markdown content, or null if not found
     */
    Response(String markdown) {
      this.markdown = markdown;
    }

    /**
     * Gets the markdown content.
     *
     * @return the markdown, or null if not found
     */
    public String getMarkdown() {
      return markdown;
    }

    /**
     * Checks if this response contains documentation.
     *
     * @return true if markdown content is present
     */
    public boolean hasContent() {
      return markdown != null && !markdown.isEmpty();
    }
  }
}
