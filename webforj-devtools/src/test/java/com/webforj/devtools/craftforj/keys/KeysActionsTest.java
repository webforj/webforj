package com.webforj.devtools.craftforj.keys;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.keys.action.GetKeysAction;
import com.webforj.devtools.craftforj.keys.action.SetKeyAction;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.ECGenParameterSpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;
import java.util.Map;
import javax.crypto.Cipher;
import javax.crypto.KeyAgreement;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class KeysActionsTest {

  private static final Gson GSON = new Gson();
  private static final Type MAP_TYPE = new TypeToken<Map<String, String>>() {}.getType();

  @TempDir
  Path tempDir;

  private CraftforjKeyStore store;
  private KeyTransport transport;
  private GetKeysAction getAction;
  private SetKeyAction setAction;
  private KeyPair clientKeyPair;

  @BeforeEach
  void setUp() throws Exception {
    store = new CraftforjKeyStore(tempDir.resolve("keys.json"));
    transport = new KeyTransport();
    getAction = new GetKeysAction(store, transport);
    setAction = new SetKeyAction(store, transport);

    KeyPairGenerator generator = KeyPairGenerator.getInstance("EC");
    generator.initialize(new ECGenParameterSpec("secp256r1"));
    clientKeyPair = generator.generateKeyPair();
  }

  @Test
  @DisplayName("Should return correct action names with keys prefix")
  void shouldReturnCorrectActionNames() {
    assertEquals("keys.get", getAction.getAction());
    assertEquals("keys.set", setAction.getAction());
  }

  @Test
  @DisplayName("Should store a sealed value and return it sealed for the client key")
  void shouldRoundTripSealedValue() throws Exception {
    setAction.handle(sealedSetParams("ai.openai", "sk-test-123"));

    GetKeysAction.Response response = getAction.handle(getParams());
    Map<String, String> keys = unseal(response);

    assertEquals(Map.of("ai.openai", "sk-test-123"), keys);
  }

  @Test
  @DisplayName("Should remove an entry when the value is missing")
  void shouldRemoveEntryOnMissingValue() throws Exception {
    setAction.handle(sealedSetParams("ai.openai", "sk-test-123"));

    JsonObject params = new JsonObject();
    params.addProperty("id", "ai.openai");
    SetKeyAction.Response response = setAction.handle(params);

    assertTrue(response.getIds().isEmpty());
    assertTrue(store.getAll().isEmpty());
  }

  @Test
  @DisplayName("Should persist entries across store instances")
  void shouldPersistAcrossInstances() throws Exception {
    setAction.handle(sealedSetParams("ai.anthropic", "sk-ant-1"));

    CraftforjKeyStore reloaded = new CraftforjKeyStore(tempDir.resolve("keys.json"));

    assertEquals(Map.of("ai.anthropic", "sk-ant-1"), reloaded.getAll());
  }

  @Test
  @DisplayName("Should reject a get request without a client public key")
  void shouldRejectGetWithoutPublicKey() {
    assertThrows(CraftforjActionException.class, () -> getAction.handle(new JsonObject()));
  }

  @Test
  @DisplayName("Should reject a get request with a malformed public key")
  void shouldRejectMalformedPublicKey() {
    JsonObject params = new JsonObject();
    params.addProperty("publicKey", "not-a-key");

    assertThrows(CraftforjActionException.class, () -> getAction.handle(params));
  }

  @Test
  @DisplayName("Should reject a set request whose sealed value was tampered with")
  void shouldRejectTamperedValue() throws Exception {
    JsonObject params = sealedSetParams("ai.openai", "sk-test-123");
    byte[] sealed = Base64.getDecoder().decode(params.get("value").getAsString());
    sealed[0] ^= 0x1;
    params.addProperty("value", Base64.getEncoder().encodeToString(sealed));

    assertThrows(CraftforjActionException.class, () -> setAction.handle(params));
    assertTrue(store.getAll().isEmpty());
  }

  @Test
  @DisplayName("Should start empty when the store file is corrupt")
  void shouldStartEmptyOnCorruptFile() throws Exception {
    Path corrupt = tempDir.resolve("corrupt.json");
    Files.writeString(corrupt, "{not-json");

    assertTrue(new CraftforjKeyStore(corrupt).getAll().isEmpty());
  }

  private JsonObject getParams() {
    JsonObject params = new JsonObject();
    params.addProperty("publicKey", encodeClientPublicKey());

    return params;
  }

  private JsonObject sealedSetParams(String id, String value) throws Exception {
    byte[] iv = new byte[12];
    new SecureRandom().nextBytes(iv);
    Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
    cipher.init(Cipher.ENCRYPT_MODE, sharedKey(transport.getPublicKey()),
        new GCMParameterSpec(128, iv));
    byte[] sealed = cipher.doFinal(value.getBytes(StandardCharsets.UTF_8));

    JsonObject params = new JsonObject();
    params.addProperty("id", id);
    params.addProperty("publicKey", encodeClientPublicKey());
    params.addProperty("iv", Base64.getEncoder().encodeToString(iv));
    params.addProperty("value", Base64.getEncoder().encodeToString(sealed));

    return params;
  }

  private Map<String, String> unseal(GetKeysAction.Response response) throws Exception {
    Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
    cipher.init(Cipher.DECRYPT_MODE, sharedKey(response.getServerPublicKey()),
        new GCMParameterSpec(128, Base64.getDecoder().decode(response.getIv())));
    byte[] plain = cipher.doFinal(Base64.getDecoder().decode(response.getPayload()));

    return GSON.fromJson(new String(plain, StandardCharsets.UTF_8), MAP_TYPE);
  }

  private SecretKeySpec sharedKey(String serverPublicKey) throws Exception {
    KeyFactory factory = KeyFactory.getInstance("EC");
    PublicKey serverKey =
        factory.generatePublic(new X509EncodedKeySpec(Base64.getDecoder().decode(serverPublicKey)));
    KeyAgreement agreement = KeyAgreement.getInstance("ECDH");
    agreement.init(clientKeyPair.getPrivate());
    agreement.doPhase(serverKey, true);
    byte[] digest = MessageDigest.getInstance("SHA-256").digest(agreement.generateSecret());

    return new SecretKeySpec(digest, "AES");
  }

  private String encodeClientPublicKey() {
    return Base64.getEncoder().encodeToString(clientKeyPair.getPublic().getEncoded());
  }
}
