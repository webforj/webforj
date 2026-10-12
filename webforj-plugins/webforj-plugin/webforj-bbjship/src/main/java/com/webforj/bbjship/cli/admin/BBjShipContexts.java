package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminWebAppServer;
import com.basis.api.admin.BBjAdminWebAppServer_1500;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.bbjship.BBjShipServlet;
import com.webforj.bbjship.model.BBjShipVersion;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.net.Authenticator;
import java.net.HttpURLConnection;
import java.net.PasswordAuthentication;
import java.net.Socket;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.function.Predicate;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509ExtendedTrustManager;

/**
 * Configures the web contexts of BBjShip in BBjServices.
 *
 * <p>
 * Each context serves one BBx servlet mapped to every path of the context. A context is owned by
 * BBjShip when its document base is the folder BBjShip gives it. A context placed on a port where
 * the BBjServices web server does not answer is taken back.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipContexts {

  /**
   * Names the web contexts of BBjShip.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private enum Context {
    // @formatter:off
    BBJSHIP("webforj-bbjship", "/webforj-bbjship", "htdocs", "WebforjBBjDeploymentServlet",
        "BBjShip"),
    ASSETS("webforj-bbjship-assets", "/webforj", "assets", "WebforjBBjAssetsServlet", "assets");
    // @formatter:on

    private final String contextName;
    private final String path;
    private final String documentFolder;
    private final String servlet;
    private final String label;

    Context(String contextName, String path, String documentFolder, String servlet, String label) {
      this.contextName = contextName;
      this.path = path;
      this.documentFolder = documentFolder;
      this.servlet = servlet;
      this.label = label;
    }
  }

  /**
   * Names the listeners of the BBjServices web server a context is served on.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private enum Listener {
    HTTP("http", "HTTP"), SSL("https", "SSL");

    private final String scheme;
    private final String label;

    Listener(String scheme, String label) {
      this.scheme = scheme;
      this.label = label;
    }
  }

  private static final String DATA = BBjAdminContextInfo.JSON_DATA;
  private static final String DOCUMENT_BASE = "Document Base";
  private static final String ENABLED = "ENABLED";
  private static final String HTTP_PORT = "HTTP port";
  private static final String PROTOCOL = "Protocol";
  private static final String SSL_PORT = "SSL port";
  private static final String UNSET_SSL_PORT = "";
  private static final String SERVLETS = "BBx Servlets";
  private static final String PARAMETERS = "Parameters";
  private static final Duration ANSWER_TIMEOUT = Duration.ofSeconds(30);
  private static final Duration ANSWER_DEADLINE = Duration.ofSeconds(15);
  private static final Duration ANSWER_INTERVAL = Duration.ofMillis(250);
  private static final Gson GSON =
      new GsonBuilder().disableHtmlEscaping().setPrettyPrinting().create();

  private final BBjAdminBase admin;
  private final BBjShipPlugin plugin;
  private final Duration answerDeadline;

  /**
   * Creates the contexts of BBjShip.
   *
   * @param admin the administration session
   * @param plugin the BBjShip plugin of the BBj installation
   */
  public BBjShipContexts(BBjAdminBase admin, BBjShipPlugin plugin) {
    this(admin, plugin, ANSWER_DEADLINE);
  }

  BBjShipContexts(BBjAdminBase admin, BBjShipPlugin plugin, Duration answerDeadline) {
    this.admin = admin;
    this.plugin = plugin;
    this.answerDeadline = answerDeadline;
  }

  /**
   * Creates or updates the BBjShip context and moves the assets context to its ports, then starts
   * or restarts both.
   *
   * <p>
   * The contexts serve HTTP on the port and HTTPS on the SSL port when one is given. When
   * BBjServices refuses the configuration or a context does not answer on a port afterward, both
   * contexts get back the configuration they had, and a BBjShip context that did not exist is
   * removed. When the BBjShip context fails, the assets context restarts with the configuration it
   * has. An assets context started in this session on the ports of the BBjShip context is left as
   * it is.
   * </p>
   *
   * @param port the HTTP port from 1 to 65535
   * @param sslPort the SSL port from 1 to 65535, or empty
   * @param assetsStarted whether the assets context was created and started in this session
   *
   * @throws IOException if a required file is missing, a context belongs to another application,
   *         does not start or does not answer on a port
   * @throws BBjAdminException if BBjServices refuses the configuration
   * @throws InterruptedException if the thread is interrupted while waiting for the answer
   */
  public void enable(int port, OptionalInt sslPort, boolean assetsStarted)
      throws IOException, BBjAdminException, InterruptedException {
    BBjAdminWebAppServer_1500 server = getServer();
    Optional<String> previous = findOwnedData(server, Context.BBJSHIP);
    Optional<String> assets = findOwnedData(server, Context.ASSETS);
    boolean assetsRestart = assets.isPresent() && !assetsStarted;
    JsonObject ports = createPorts(port, sslPort);
    try {
      applyCheckedContext(server, Context.BBJSHIP, previous, ports,
          BBjShipContexts::isBBjShipChallenge);
    } catch (IOException | BBjAdminException | InterruptedException | RuntimeException failure) {
      if (assetsRestart) {
        startContext(server, Context.ASSETS, failure);
      }

      throw failure;
    }

    if (assets.isEmpty()) {
      return;
    }

    JsonObject data = JsonParser.parseString(assets.get()).getAsJsonObject();
    boolean moved = !ports.entrySet().stream()
        .allMatch(field -> field.getValue().equals(data.get(field.getKey())));
    try {
      if (moved) {
        applyCheckedContext(server, Context.ASSETS, assets, ports, answer -> true);
      } else if (assetsRestart) {
        startContext(server, Context.ASSETS);
      }
    } catch (IOException | BBjAdminException | InterruptedException | RuntimeException failure) {
      resetContext(server, Context.BBJSHIP, previous, failure);
      throw failure;
    }
  }

  /**
   * Returns the local addresses of the BBjShip context, the HTTP address and the HTTPS address when
   * the context has an SSL port.
   *
   * @return the addresses, or an empty list when the BBjShip context does not exist
   * @throws IOException if the context belongs to another application
   * @throws BBjAdminException if BBjServices refuses the request
   */
  public List<URI> getAddresses() throws IOException, BBjAdminException {
    BBjAdminWebAppServer_1500 server = getServer();
    if (!hasOwnedContext(server, Context.BBJSHIP)) {
      return List.of();
    }

    JsonObject data = readData(server.getContextInfo(Context.BBJSHIP.contextName));
    List<URI> addresses = new ArrayList<>();
    addresses.add(toAddress(Listener.HTTP, data.get(HTTP_PORT).getAsInt()));
    readSslPort(data).ifPresent(port -> addresses.add(toAddress(Listener.SSL, port)));

    return List.copyOf(addresses);
  }

  /**
   * Stops and removes the BBjShip context when it exists.
   *
   * @return true when the BBjShip context existed and was removed
   * @throws IOException if the context belongs to another application or does not stop
   * @throws BBjAdminException if BBjServices refuses the removal
   */
  public boolean disable() throws IOException, BBjAdminException {
    BBjAdminWebAppServer_1500 server = getServer();
    if (!hasOwnedContext(server, Context.BBJSHIP)) {
      return false;
    }

    removeContext(server, Context.BBJSHIP);

    return true;
  }

  /**
   * Stops and removes the assets context when it exists.
   *
   * @return true when the assets context existed and was removed
   * @throws IOException if the context belongs to another application or does not stop
   * @throws BBjAdminException if BBjServices refuses the removal
   */
  public boolean removeAssets() throws IOException, BBjAdminException {
    BBjAdminWebAppServer_1500 server = getServer();
    if (!hasOwnedContext(server, Context.ASSETS)) {
      return false;
    }

    removeContext(server, Context.ASSETS);

    return true;
  }

  /**
   * Creates the document folders of the contexts of BBjShip when they are missing.
   *
   * @throws IOException if a document folder cannot be created
   */
  void createDocumentRoots() throws IOException {
    for (Context context : Context.values()) {
      createDocumentRoot(context);
    }
  }

  /**
   * Creates and starts the assets context when it does not exist.
   *
   * <p>
   * Without ports the context listens on the ports of the BBjShip context, or on the default port
   * of BBjServices when there is no BBjShip context. With a port where the BBjServices web server
   * does not answer, the context is removed again.
   * </p>
   *
   * @param httpPort the HTTP port from 1 to 65535, or empty
   * @param sslPort the SSL port from 1 to 65535, or empty
   *
   * @return true when the assets context was created and started
   * @throws IOException if a required file is missing, a context belongs to another application,
   *         the assets context does not start or nothing answers on a port
   * @throws BBjAdminException if BBjServices refuses the configuration
   * @throws InterruptedException if the thread is interrupted while waiting for the answer
   */
  boolean addAssets(OptionalInt httpPort, OptionalInt sslPort)
      throws IOException, BBjAdminException, InterruptedException {
    BBjAdminWebAppServer_1500 server = getServer();
    if (hasOwnedContext(server, Context.ASSETS)) {
      return false;
    }

    JsonObject ports =
        httpPort.isPresent() ? createPorts(httpPort.getAsInt(), sslPort) : readBBjShipPorts(server);

    applyFileValidation(Context.ASSETS);
    applyContext(server, Context.ASSETS, false, ports);
    Optional<IOException> failure = findPortFailure(httpPort, sslPort, answer -> true);
    if (failure.isPresent()) {
      removeContext(server, Context.ASSETS);
      throw failure.get();
    }

    return true;
  }

  /**
   * Returns the ports of the BBjShip context, the HTTP port and the SSL port when the context has
   * one.
   *
   * @return the ports, or an empty list when the BBjShip context does not exist
   * @throws IOException if the context belongs to another application
   * @throws BBjAdminException if BBjServices refuses the request
   */
  List<Integer> getPorts() throws IOException, BBjAdminException {
    JsonObject data = readBBjShipPorts(getServer());
    if (!data.has(HTTP_PORT)) {
      return List.of();
    }

    List<Integer> ports = new ArrayList<>();
    ports.add(data.get(HTTP_PORT).getAsInt());
    readSslPort(data).ifPresent(ports::add);

    return List.copyOf(ports);
  }

  /**
   * Asks BBjShip on the HTTP port for its version with the credentials.
   *
   * @param port the HTTP port of the BBjShip context
   * @param credentials the administration credentials
   *
   * @return the version answer, or empty when the request fails or the answer is not 200
   * @throws InterruptedException if the thread is interrupted while waiting for the answer
   */
  Optional<BBjShipVersion> fetchVersion(int port, BBjCredentials credentials)
      throws InterruptedException {
    HttpRequest request =
        HttpRequest.newBuilder(URI.create(toAddress(Listener.HTTP, port) + "/version"))
            .timeout(ANSWER_TIMEOUT).build();
    try (HttpClient client = HttpClient.newBuilder().connectTimeout(ANSWER_TIMEOUT)
        .authenticator(new CredentialsAuthenticator(credentials)).build()) {
      HttpResponse<String> answer = client.send(request, BodyHandlers.ofString());
      if (answer.statusCode() != HttpURLConnection.HTTP_OK) {
        return Optional.empty();
      }

      return Optional.of(GSON.fromJson(answer.body(), BBjShipVersion.class));
    } catch (IOException unanswered) {
      return Optional.empty();
    }
  }

  private void applyCheckedContext(BBjAdminWebAppServer_1500 server, Context context,
      Optional<String> previous, JsonObject ports, Predicate<HttpResponse<Void>> expected)
      throws IOException, BBjAdminException, InterruptedException {
    applyFileValidation(context);
    Optional<IOException> failure;
    try {
      applyContext(server, context, previous.isPresent(), ports);
      failure = findPortFailure(OptionalInt.of(ports.get(HTTP_PORT).getAsInt()), readSslPort(ports),
          expected);
    } catch (IOException | BBjAdminException | RuntimeException refusal) {
      resetContext(server, context, previous, refusal);
      throw refusal;
    }

    if (failure.isPresent()) {
      resetContext(server, context, previous, failure.get());
      throw failure.get();
    }
  }

  private void applyFileValidation(Context context) throws IOException {
    if (!Files.isRegularFile(getServletSource(context))) {
      throw new IOException(
          "The " + context.label + " servlet is missing. Check the plugin installation");
    }

    if (!Files.isRegularFile(plugin.getConfigFile())) {
      throw new IOException("The BBj configuration file is missing. Check the BBj installation");
    }

    createDocumentRoot(context);
  }

  private void createDocumentRoot(Context context) throws IOException {
    try {
      Files.createDirectories(getDocumentRoot(context));
    } catch (IOException failure) {
      throw new IOException(
          "Cannot create the " + context.label + " document directory. Check the runtime folder",
          failure);
    }
  }

  private void applyContext(BBjAdminWebAppServer_1500 server, Context context, boolean exists,
      JsonObject ports) throws IOException, BBjAdminException {
    BBjAdminContextInfo info = exists ? server.getContextInfo(context.contextName)
        : server.createContext(context.contextName);
    JsonObject data = readData(info);
    data.addProperty("Path", context.path);
    data.addProperty(DOCUMENT_BASE, getDocumentRoot(context).toString());
    ports.entrySet().forEach(field -> data.add(field.getKey(), field.getValue()));
    data.add(SERVLETS, createServlets(context, getServletSource(context), plugin.getConfigFile()));
    info.setString(DATA, GSON.toJson(data));
    info.commit();
    server.commit();
    startContext(server, context);
  }

  private void startContext(BBjAdminWebAppServer_1500 server, Context context)
      throws IOException, BBjAdminException {
    JsonObject actions = readData(server.getContextInfo(context.contextName));
    JsonObject restart = actions.getAsJsonObject("RESTART");
    sendAction(context,
        restart.get(ENABLED).getAsBoolean() ? restart : actions.getAsJsonObject("START"));
  }

  private void startContext(BBjAdminWebAppServer_1500 server, Context context, Exception failure) {
    try {
      startContext(server, context);
    } catch (IOException | BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }
  }

  private void removeContext(BBjAdminWebAppServer_1500 server, Context context)
      throws IOException, BBjAdminException {
    JsonObject stop = readData(server.getContextInfo(context.contextName)).getAsJsonObject("STOP");
    if (stop.get(ENABLED).getAsBoolean()) {
      sendAction(context, stop);
    }

    server.removeContext(context.contextName);
    server.commit();
  }

  private boolean hasOwnedContext(BBjAdminWebAppServer_1500 server, Context context)
      throws IOException, BBjAdminException {
    return findOwnedData(server, context).isPresent();
  }

  private Optional<String> findOwnedData(BBjAdminWebAppServer_1500 server, Context context)
      throws IOException, BBjAdminException {
    if (!server.getContextNames().contains(context.contextName)) {
      return Optional.empty();
    }

    String data = server.getContextInfo(context.contextName).getString(DATA);
    JsonElement base = JsonParser.parseString(data).getAsJsonObject().get(DOCUMENT_BASE);
    if (base == null || !getDocumentRoot(context).toString().equals(base.getAsString())) {
      throw new IOException("The " + context.label + " context belongs to another application");
    }

    return Optional.of(data);
  }

  private JsonObject readBBjShipPorts(BBjAdminWebAppServer_1500 server)
      throws IOException, BBjAdminException {
    JsonObject ports = new JsonObject();
    if (!hasOwnedContext(server, Context.BBJSHIP)) {
      return ports;
    }

    JsonObject bbjShip = readData(server.getContextInfo(Context.BBJSHIP.contextName));
    for (String field : List.of(HTTP_PORT, PROTOCOL, SSL_PORT)) {
      if (bbjShip.has(field)) {
        ports.add(field, bbjShip.get(field));
      }
    }

    return ports;
  }

  private BBjAdminWebAppServer_1500 getServer() throws IOException, BBjAdminException {
    BBjAdminWebAppServer server = admin.getWebAppServer();
    if (!(server instanceof BBjAdminWebAppServer_1500 contexts)) {
      throw new IOException("BBjServices does not manage web contexts. Update BBj");
    }

    return contexts;
  }

  private Path getServletSource(Context context) {
    return plugin.getDirectory().resolve("servlets").resolve(context.servlet + ".bbj");
  }

  private Path getDocumentRoot(Context context) {
    return plugin.getRuntime().resolve(context.documentFolder);
  }

  private JsonArray createServlets(Context context, Path source, Path config) {
    JsonObject servlet = new JsonObject();
    servlet.addProperty("Enabled", "true");
    servlet.addProperty("URL Mapping", "/*");
    servlet.addProperty("Source File", source.toString());
    servlet.addProperty("Class Name", context.servlet);
    servlet.addProperty("Method Name", "service");
    servlet.addProperty("Config File", config.toString());
    servlet.addProperty("SSCP", BBjShipClasspaths.SERVLET_CLASSPATH);
    servlet.addProperty("Terminal Alias", "");
    servlet.addProperty("Working Directory", "");
    servlet.addProperty("User", "");
    servlet.add(PARAMETERS, new JsonArray());
    JsonArray servlets = new JsonArray();
    servlets.add(servlet);

    return servlets;
  }

  private void sendAction(Context context, JsonObject action)
      throws IOException, BBjAdminException {
    JsonObject answer =
        JsonParser.parseString(admin.jsonExecute(GSON.toJson(action))).getAsJsonObject();
    if (answer.has("MESSAGE")) {
      throw new IOException("The " + context.label + " context did not "
          + action.getAsJsonObject("DATA").get("ACTION").getAsString().toLowerCase(Locale.ROOT)
          + ". " + answer.get("MESSAGE").getAsString());
    }
  }

  private static JsonObject readData(BBjAdminContextInfo info) throws BBjAdminException {
    return JsonParser.parseString(info.getString(DATA)).getAsJsonObject();
  }

  private Optional<IOException> findPortFailure(OptionalInt httpPort, OptionalInt sslPort,
      Predicate<HttpResponse<Void>> expected) throws InterruptedException {
    if (httpPort.isPresent()
        && fetchVersionAnswer(Listener.HTTP, httpPort.getAsInt(), expected).isEmpty()) {
      return Optional.of(createPortFailure(Listener.HTTP, httpPort.getAsInt()));
    }

    if (sslPort.isPresent()
        && fetchVersionAnswer(Listener.SSL, sslPort.getAsInt(), expected).isEmpty()) {
      return Optional.of(createPortFailure(Listener.SSL, sslPort.getAsInt()));
    }

    return Optional.empty();
  }

  /**
   * Asks BBjShip for its version until the expected answer arrives or the deadline passes.
   *
   * <p>
   * Over HTTPS the request accepts any server certificate without a host name check, which is
   * acceptable only because it is a loopback request to this BBjServices that carries no
   * credentials and expects the authentication challenge.
   * </p>
   */
  private Optional<HttpResponse<Void>> fetchVersionAnswer(Listener listener, int port,
      Predicate<HttpResponse<Void>> expected) throws InterruptedException {
    HttpRequest request = HttpRequest.newBuilder(URI.create(toAddress(listener, port) + "/version"))
        .timeout(ANSWER_TIMEOUT).build();
    long deadline = System.nanoTime() + answerDeadline.toNanos();
    HttpClient.Builder builder = HttpClient.newBuilder().connectTimeout(ANSWER_TIMEOUT);
    if (listener == Listener.SSL) {
      builder.sslContext(createLoopbackSslContext());
    }

    try (HttpClient client = builder.build()) {
      HttpResponse<Void> answer = client.send(request, BodyHandlers.discarding());
      while (!expected.test(answer) && System.nanoTime() - deadline < 0) {
        Thread.sleep(ANSWER_INTERVAL);
        answer = client.send(request, BodyHandlers.discarding());
      }

      return Optional.of(answer).filter(expected);
    } catch (IOException unanswered) {
      return Optional.empty();
    }
  }

  private static boolean isBBjShipChallenge(HttpResponse<Void> answer) {
    return answer.statusCode() == 401 && answer.headers().allValues("WWW-Authenticate").stream()
        .flatMap(value -> Arrays.stream(value.split(","))).map(String::trim)
        .anyMatch(BBjShipServlet.BASIC_CHALLENGE::equals);
  }

  private void resetContext(BBjAdminWebAppServer_1500 server, Context context,
      Optional<String> previous, Exception failure) {
    try {
      if (previous.isPresent()) {
        BBjAdminContextInfo info = server.getContextInfo(context.contextName);
        info.setString(DATA, previous.get());
        info.commit();
        server.commit();
        startContext(server, context);
      } else {
        removeContext(server, context);
      }
    } catch (IOException | BBjAdminException | RuntimeException cleanup) {
      failure.addSuppressed(cleanup);
    }
  }

  private static JsonObject createPorts(int httpPort, OptionalInt sslPort) {
    JsonObject ports = new JsonObject();
    ports.addProperty(HTTP_PORT, httpPort);
    ports.addProperty(PROTOCOL, true);
    if (sslPort.isPresent()) {
      ports.addProperty(SSL_PORT, sslPort.getAsInt());
    } else {
      ports.addProperty(SSL_PORT, UNSET_SSL_PORT);
    }

    return ports;
  }

  private static OptionalInt readSslPort(JsonObject data) {
    JsonElement value = data.get(SSL_PORT);
    if (value == null || !value.isJsonPrimitive()) {
      return OptionalInt.empty();
    }

    try {
      int port = Integer.parseInt(value.getAsString().strip());

      return port < 1 ? OptionalInt.empty() : OptionalInt.of(port);
    } catch (NumberFormatException unset) {
      return OptionalInt.empty();
    }
  }

  private static URI toAddress(Listener listener, int port) {
    return URI.create(listener.scheme + "://localhost:" + port + Context.BBJSHIP.path);
  }

  private static SSLContext createLoopbackSslContext() {
    try {
      SSLContext context = SSLContext.getInstance("TLS");
      context.init(null, new TrustManager[] {new LoopbackTrustManager()}, null);

      return context;
    } catch (GeneralSecurityException failure) {
      throw new IllegalStateException("The Java runtime does not support TLS", failure);
    }
  }

  private static IOException createPortFailure(Listener listener, int port) {
    return new IOException("The BBjServices web server does not answer on port " + port
        + ". Use the " + listener.label + " port of the BBjServices web server");
  }

  /**
   * Accepts every server certificate of the loopback version request.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class LoopbackTrustManager extends X509ExtendedTrustManager {

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkClientTrusted(X509Certificate[] chain, String authType)
        throws CertificateException {
      throw new CertificateException("The loopback version request accepts no client");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkClientTrusted(X509Certificate[] chain, String authType, Socket socket)
        throws CertificateException {
      throw new CertificateException("The loopback version request accepts no client");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkClientTrusted(X509Certificate[] chain, String authType, SSLEngine engine)
        throws CertificateException {
      throw new CertificateException("The loopback version request accepts no client");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkServerTrusted(X509Certificate[] chain, String authType) {}

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkServerTrusted(X509Certificate[] chain, String authType, Socket socket) {}

    /**
     * {@inheritDoc}
     */
    @Override
    public void checkServerTrusted(X509Certificate[] chain, String authType, SSLEngine engine) {}

    /**
     * {@inheritDoc}
     */
    @Override
    public X509Certificate[] getAcceptedIssuers() {
      return new X509Certificate[0];
    }
  }

  /**
   * Answers the authentication challenge of BBjShip with the administration credentials.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  private static final class CredentialsAuthenticator extends Authenticator {

    private final BBjCredentials credentials;

    CredentialsAuthenticator(BBjCredentials credentials) {
      this.credentials = credentials;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PasswordAuthentication getPasswordAuthentication() {
      return new PasswordAuthentication(credentials.getUsername(),
          credentials.getPassword().toCharArray());
    }
  }
}
