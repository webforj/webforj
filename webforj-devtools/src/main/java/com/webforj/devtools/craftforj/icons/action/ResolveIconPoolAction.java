package com.webforj.devtools.craftforj.icons.action;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Action handler that resolves icon names to their artwork sources through the running page's own
 * pool resolvers.
 *
 * <p>
 * Resolution happens in the browser through the {@code window.Dwc.IconsPools} resolvers of the
 * running page, one batched call per pool.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ResolveIconPoolAction implements CraftforjActionHandler<PendingResult<Object>> {

  /** The action name. */
  public static final String ACTION = "icons.resolve";

  private static final Gson GSON = new Gson();
  private static final String PARAM_NAMES = "names";

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
  public PendingResult<Object> handle(JsonObject params) {
    String pool = params != null && params.has("pool") ? params.get("pool").getAsString() : null;
    List<String> names = readNames(params);
    if (pool == null || pool.isBlank() || names.isEmpty()) {
      throw new CraftforjActionException("Missing pool or names");
    }

    return getPage().executeJsAsync(buildScript(pool, names)).thenApply(this::toResponse);
  }

  /**
   * Gets the current page. Extracted for testing.
   *
   * @return the current page
   */
  protected Page getPage() {
    return Page.getCurrent();
  }

  private static List<String> readNames(JsonObject params) {
    List<String> names = new ArrayList<>();
    if (params != null && params.has(PARAM_NAMES) && params.get(PARAM_NAMES).isJsonArray()) {
      JsonArray array = params.getAsJsonArray(PARAM_NAMES);
      array.forEach(element -> names.add(element.getAsString()));
    }

    return names;
  }

  private static String buildScript(String pool, List<String> names) {
    return """
        (() => {
          const pool = ((window.Dwc && window.Dwc.IconsPools) || []).find(p => p.name === %s);
          if (!pool) return null;

          const out = {};
          for (const name of %s) {
            try {
              out[name] = String(pool.resolver(name) || '');
            } catch (e) {
              out[name] = '';
            }
          }

          return JSON.stringify(out);
        })()""".formatted(GSON.toJson(pool), GSON.toJson(names));
  }

  private Response toResponse(Object result) {
    if (!(result instanceof String json) || json.isBlank() || "null".equals(json)) {
      return new Response(Map.of());
    }

    Map<String, String> icons = GSON.fromJson(json, new TypeToken<Map<String, String>>() {});

    return new Response(icons == null ? Map.of() : icons);
  }

  /**
   * Response containing the resolved icon sources keyed by icon name.
   */
  public static class Response {
    private final Map<String, String> icons;

    Response(Map<String, String> icons) {
      this.icons = icons;
    }

    /**
     * Gets the resolved icon sources.
     *
     * @return the icon sources keyed by name
     */
    public Map<String, String> getIcons() {
      return icons;
    }
  }
}
