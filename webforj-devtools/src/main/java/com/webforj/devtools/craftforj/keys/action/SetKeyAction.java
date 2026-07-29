package com.webforj.devtools.craftforj.keys.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.keys.CraftforjKeyStore;
import com.webforj.devtools.craftforj.keys.KeyTransport;
import java.util.List;

/**
 * Action handler that stores or removes one secret.
 *
 * <p>
 * The value arrives sealed with the shared key derived from the client's ephemeral public key and
 * the server transport key. A missing or blank value removes the entry.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class SetKeyAction implements CraftforjActionHandler<SetKeyAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "keys.set";

  private static final String PARAM_VALUE = "value";
  private final CraftforjKeyStore store;
  private final KeyTransport transport;

  /**
   * Creates a new SetKeyAction.
   *
   * @param store the secret store
   * @param transport the encryption transport shared with {@code GetKeysAction}
   */
  public SetKeyAction(CraftforjKeyStore store, KeyTransport transport) {
    this.store = store;
    this.transport = transport;
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
    String id = KeysParams.requireString(params, "id");
    String value = resolveValue(params);

    if (value == null || value.isBlank()) {
      store.remove(id);
    } else {
      store.set(id, value);
    }

    return new Response(store.getIds());
  }

  private String resolveValue(JsonObject params) {
    if (params == null || !params.has(PARAM_VALUE) || params.get(PARAM_VALUE).isJsonNull()) {
      return null;
    }

    String clientPublicKey = KeysParams.requireString(params, "publicKey");
    String iv = KeysParams.requireString(params, "iv");
    try {
      return transport.decrypt(clientPublicKey, iv, params.get(PARAM_VALUE).getAsString());
    } catch (IllegalStateException e) {
      throw new CraftforjActionException("Failed to unseal key value", e);
    }
  }

  /**
   * Response containing the ids now held in the store.
   */
  public static class Response {

    private final List<String> ids;

    Response(List<String> ids) {
      this.ids = ids;
    }

    /**
     * Gets the stored entry ids.
     *
     * @return the ids, without values
     */
    public List<String> getIds() {
      return ids;
    }
  }
}
