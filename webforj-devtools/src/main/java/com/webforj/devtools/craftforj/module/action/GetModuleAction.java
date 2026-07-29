package com.webforj.devtools.craftforj.module.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.module.ModuleStore;
import com.webforj.devtools.craftforj.module.model.ModuleSource;
import java.util.HashMap;
import java.util.Map;

/**
 * Action handler that serves a craftforJ client module by name.
 *
 * <p>
 * The boot script injected into the page asks for a module by the name it knows it as, and receives
 * the base64 payload a chunk at a time along with its digest. That digest is stamped inline in the
 * same boot script, so the client can tell an answer matching what its page was built against from
 * one that does not.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetModuleAction implements CraftforjActionHandler<Map<String, Object>> {

  private static final int DEFAULT_LENGTH = 1048576;

  private final ModuleStore store;

  /**
   * Creates a handler serving the modules the jar ships.
   */
  public GetModuleAction() {
    this(new ModuleStore());
  }

  /**
   * Creates a handler serving the given modules.
   *
   * @param store the modules to serve
   */
  public GetModuleAction(ModuleStore store) {
    this.store = store;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return "devtools.getModule";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Map<String, Object> handle(JsonObject params) {
    if (params == null || !params.has("name")) {
      throw new CraftforjActionException("A craftforJ module request carries no name");
    }

    ModuleSource module = store.read(params.get("name").getAsString());
    String encoded = module.getBase64();
    int offset = params.has("offset") ? params.get("offset").getAsInt() : 0;
    int length = params.has("length") ? params.get("length").getAsInt() : DEFAULT_LENGTH;

    int total = encoded.length();
    int end = Math.min(total, offset + length);
    String chunk = offset >= total ? "" : encoded.substring(offset, end);

    Map<String, Object> response = new HashMap<>();
    response.put("total", total);
    response.put("sha256", module.getSha256());
    response.put("chunk", chunk);

    return response;
  }
}
