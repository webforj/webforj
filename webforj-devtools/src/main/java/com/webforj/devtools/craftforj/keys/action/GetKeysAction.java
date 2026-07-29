package com.webforj.devtools.craftforj.keys.action;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.keys.CraftforjKeyStore;
import com.webforj.devtools.craftforj.keys.KeyTransport;

/**
 * Action handler that returns all stored secrets, sealed for the requesting client.
 *
 * <p>
 * The client sends an ephemeral public key and receives the secret map encrypted with the shared
 * key derived from it and the server transport key.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class GetKeysAction implements CraftforjActionHandler<GetKeysAction.Response> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "keys.get";

  private static final Gson GSON = new Gson();

  private final CraftforjKeyStore store;
  private final KeyTransport transport;

  /**
   * Creates a new GetKeysAction.
   *
   * @param store the secret store
   * @param transport the encryption transport shared with {@code SetKeyAction}
   */
  public GetKeysAction(CraftforjKeyStore store, KeyTransport transport) {
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
    String clientPublicKey = KeysParams.requireString(params, "publicKey");
    KeyTransport.Sealed sealed;
    try {
      sealed = transport.encrypt(clientPublicKey, GSON.toJson(store.getAll()));
    } catch (IllegalStateException e) {
      throw new CraftforjActionException("Invalid client public key", e);
    }

    return new Response(transport.getPublicKey(), sealed.getIv(), sealed.getPayload());
  }

  /**
   * Response containing the sealed secret map.
   */
  public static class Response {

    private final String serverPublicKey;
    private final String iv;
    private final String payload;

    Response(String serverPublicKey, String iv, String payload) {
      this.serverPublicKey = serverPublicKey;
      this.iv = iv;
      this.payload = payload;
    }

    /**
     * Gets the server's SPKI-encoded public key.
     *
     * @return the public key, base64
     */
    public String getServerPublicKey() {
      return serverPublicKey;
    }

    /**
     * Gets the GCM initialization vector.
     *
     * @return the iv, base64
     */
    public String getIv() {
      return iv;
    }

    /**
     * Gets the sealed JSON map of secrets.
     *
     * @return the ciphertext, base64
     */
    public String getPayload() {
      return payload;
    }
  }
}
