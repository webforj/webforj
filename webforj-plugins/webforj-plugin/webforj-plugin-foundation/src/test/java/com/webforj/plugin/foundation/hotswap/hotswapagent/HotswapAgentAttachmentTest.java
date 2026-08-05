package com.webforj.plugin.foundation.hotswap.hotswapagent;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.sun.net.httpserver.HttpServer;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HotswapAgentAttachmentTest {

  private static final byte[] JAR_BYTES = "agent bytes".getBytes(StandardCharsets.UTF_8);

  private HttpServer server;
  private String host;
  private final AtomicInteger jarRequests = new AtomicInteger();
  private String servedChecksum;

  @BeforeEach
  void startRepository() throws Exception {
    servedChecksum = sha1(JAR_BYTES);
    server = HttpServer.create(new InetSocketAddress("localhost", 0), 0);
    server.createContext("/", exchange -> {
      byte[] body;
      if (exchange.getRequestURI().getPath().endsWith(".sha1")) {
        body = servedChecksum.getBytes(StandardCharsets.UTF_8);
      } else {
        jarRequests.incrementAndGet();
        body = JAR_BYTES;
      }
      exchange.sendResponseHeaders(200, body.length);
      try (OutputStream out = exchange.getResponseBody()) {
        out.write(body);
      }
    });
    server.start();
    host = "http://localhost:" + server.getAddress().getPort();
  }

  @AfterEach
  void stopRepository() {
    server.stop(0);
  }

  @Test
  void shouldDownloadVerifyAndCacheTheJar(@TempDir Path tmp) throws Exception {
    HotswapAgentAttachment attachment = newAttachment(tmp).setVersion("9.9.9").build();

    List<String> arguments = attachment.arguments();

    Path jar = tmp.resolve("cache/9.9.9/hotswap-agent-9.9.9.jar");
    assertArrayEquals(JAR_BYTES, Files.readAllBytes(jar), "the verified jar lands in the cache");
    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"),
        "the proven capability is switched on for the run");
    assertTrue(
        arguments.stream()
            .anyMatch(argument -> argument.startsWith("-javaagent:" + jar.toAbsolutePath())),
        "the agent flag names the cached jar");

    attachment.arguments();
    assertEquals(1, jarRequests.get(), "the second attachment is served from the cache");
  }

  @Test
  void shouldRejectTheJarThatDoesNotMatchTheChecksum(@TempDir Path tmp) throws Exception {
    servedChecksum = sha1("other bytes".getBytes(StandardCharsets.UTF_8));
    HotswapAgentAttachment attachment = newAttachment(tmp).setVersion("9.9.9").build();

    IOException failure = assertThrows(IOException.class, attachment::arguments);

    assertTrue(failure.getMessage().contains("checksum"));
    assertTrue(listFiles(tmp.resolve("cache")).stream()
        .noneMatch(file -> file.toString().endsWith(".jar")), "no jar is left in the cache");
  }

  @Test
  void shouldComposeTheArgumentsFromTheJarAlreadyOnDisk(@TempDir Path tmp) throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    HotswapAgentAttachment attachment = newAttachment(tmp).setOverridePath(local).build();

    List<String> arguments = attachment.arguments();

    assertTrue(arguments.contains("-javaagent:" + local.toAbsolutePath()
        + "=autoHotswap=true,LOGGER=warning," + "propertiesFilePath="
        + tmp.resolve("hotswap/hotswap-agent.properties").toAbsolutePath()));
    assertTrue(
        arguments.containsAll(List.of("--add-opens=java.base/java.lang=ALL-UNNAMED",
            "--add-opens=java.base/java.io=ALL-UNNAMED",
            "--add-opens=java.desktop/java.beans=ALL-UNNAMED")),
        "the agent needs the reflective opens as self contained tokens");
    assertEquals(0, jarRequests.get(), "nothing is downloaded for an override");
  }

  @Test
  void shouldWriteTheAgentConfiguration(@TempDir Path tmp) throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    newAttachment(tmp).setOverridePath(local).build().arguments();

    String configuration = Files.readString(tmp.resolve("hotswap/hotswap-agent.properties"));
    assertTrue(configuration.contains("pluginPackages=com.webforj.devtools.hotswap"),
        "the forwarder package is discovered");
  }

  @Test
  void shouldFailWhenTheOverridePathDoesNotExist(@TempDir Path tmp) throws Exception {
    HotswapAgentAttachment attachment =
        newAttachment(tmp).setOverridePath(tmp.resolve("missing.jar")).build();

    assertThrows(IOException.class, attachment::arguments);
  }

  @Test
  void shouldWarnAndAttachLimitedOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment = HotswapAgentAttachment.create()
        .setCacheRoot(tmp.resolve("cache")).setConfigurationDirectory(tmp.resolve("hotswap"))
        .setRepositoryHost(host).setOverridePath(local).setJavaExecutable(fakeJava(tmp, 1))
        .setWarn(warnings::add).build();

    List<String> arguments = attachment.arguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("-javaagent:")),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals),
        "the unsupported flag never reaches the virtual machine");
    assertTrue(arguments.contains(HotswapAgentAttachment.TOOL_ARGUMENT));
    assertTrue(arguments.contains(HotswapAgentAttachment.LEVEL_ARGUMENT_PREFIX + "limited"),
        "the application learns the limited depth of this run");
    String warning = String.join("\n", warnings);
    assertTrue(warning.contains("####"), "the warning frames itself so it cannot be missed");
    assertTrue(warning.contains("method body changes"));
    assertTrue(warning.contains("-XX:+AllowEnhancedClassRedefinition"),
        "the requirement is named for the user");
  }

  @Test
  void shouldWarnAndAttachLimitedWhenTheJavaExecutableIsMissing(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment = HotswapAgentAttachment.create()
        .setCacheRoot(tmp.resolve("cache")).setConfigurationDirectory(tmp.resolve("hotswap"))
        .setRepositoryHost(host).setOverridePath(local)
        .setJavaExecutable(tmp.resolve("missing-java")).setWarn(warnings::add).build();

    List<String> arguments = attachment.arguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("-javaagent:")));
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals));
    assertTrue(String.join("\n", warnings).contains("java executable"));
  }

  @Test
  void shouldAskTheRunningVirtualMachineWhenNoExecutableIsNamed(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment = HotswapAgentAttachment.create()
        .setCacheRoot(tmp.resolve("cache")).setConfigurationDirectory(tmp.resolve("hotswap"))
        .setRepositoryHost(host).setOverridePath(local)
        .setRunningVirtualMachineOptions(HotswapAgentAttachment.REDEFINITION_OPTION::equals)
        .setWarn(warnings::add).build();

    List<String> arguments = attachment.arguments();

    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"),
        "the running virtual machine answered the capability itself");
    assertTrue(arguments.contains(HotswapAgentAttachment.TOOL_ARGUMENT));
    assertTrue(arguments.contains(HotswapAgentAttachment.LEVEL_ARGUMENT_PREFIX + "full"),
        "the application learns the full depth of this run");
    assertTrue(warnings.isEmpty());
  }

  @Test
  void shouldWarnAndAttachLimitedWhenTheRunningVirtualMachineLacksTheOption(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve("hotswap-agent.jar"), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment = HotswapAgentAttachment.create()
        .setCacheRoot(tmp.resolve("cache")).setConfigurationDirectory(tmp.resolve("hotswap"))
        .setRepositoryHost(host).setOverridePath(local)
        .setRunningVirtualMachineOptions(option -> false).setWarn(warnings::add).build();

    List<String> arguments = attachment.arguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("-javaagent:")),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals));
    assertTrue(String.join("\n", warnings).contains("method body changes"));
  }

  @Test
  void shouldFallBackToTheDefaultVersion(@TempDir Path tmp) throws Exception {
    assertEquals(HotswapAgentAttachment.DEFAULT_VERSION,
        newAttachment(tmp).setVersion(" ").build().getVersion());
  }

  private HotswapAgentAttachment.Builder newAttachment(Path tmp) throws IOException {
    return HotswapAgentAttachment.create().setCacheRoot(tmp.resolve("cache"))
        .setConfigurationDirectory(tmp.resolve("hotswap")).setRepositoryHost(host)
        .setJavaExecutable(fakeJava(tmp, 0));
  }

  private static Path fakeJava(Path dir, int exitCode) throws IOException {
    Assumptions.assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"),
        "the capability check stand in needs a posix file system");

    Path script = dir.resolve("java-" + exitCode);
    Files.writeString(script, "#!/bin/sh\nexit " + exitCode + "\n");
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"));

    return script;
  }

  private static String sha1(byte[] bytes) {
    try {
      return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-1").digest(bytes));
    } catch (Exception e) {
      throw new IllegalStateException(e);
    }
  }

  private static List<Path> listFiles(Path root) {
    if (!Files.isDirectory(root)) {
      return List.of();
    }

    try (var stream = Files.walk(root)) {
      return stream.filter(Files::isRegularFile).toList();
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
  }
}
