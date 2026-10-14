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
  private static final String REDEFINITION_ARGUMENT = "-XX:+AllowEnhancedClassRedefinition";
  private static final String JAVAAGENT_PREFIX = "-javaagent:";
  private static final String CACHE_FOLDER = "cache";
  private static final String AGENT_JAR = "hotswap-agent.jar";
  private static final String HOTSWAP_FOLDER = "hotswap";
  private static final String OPTIONS_FILE = "hotswap.options";

  private HttpServer server;
  private String host;
  private final AtomicInteger jarRequests = new AtomicInteger();
  private String servedChecksum;

  @BeforeEach
  void startRepository() throws IOException {
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

    List<String> arguments = attachment.getArguments();

    Path jar = tmp.resolve("cache/9.9.9/hotswap-agent-9.9.9.jar");
    assertArrayEquals(JAR_BYTES, Files.readAllBytes(jar), "the verified jar lands in the cache");
    assertTrue(arguments.contains(REDEFINITION_ARGUMENT),
        "the proven capability is switched on for the run");
    assertTrue(
        arguments.stream()
            .anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX + jar.toAbsolutePath())),
        "the agent flag names the cached jar");

    attachment.getArguments();
    assertEquals(1, jarRequests.get(), "the second attachment is served from the cache");
  }

  @Test
  void shouldRejectTheJarThatDoesNotMatchTheChecksum(@TempDir Path tmp) throws Exception {
    servedChecksum = sha1("other bytes".getBytes(StandardCharsets.UTF_8));
    HotswapAgentAttachment attachment = newAttachment(tmp).setVersion("9.9.9").build();

    IOException failure = assertThrows(IOException.class, attachment::getArguments);

    assertTrue(failure.getMessage().contains("checksum"));
    assertTrue(listFiles(tmp.resolve(CACHE_FOLDER)).stream()
        .noneMatch(file -> file.toString().endsWith(".jar")), "no jar is left in the cache");
  }

  @Test
  void shouldComposeTheArgumentsFromTheJarAlreadyOnDisk(@TempDir Path tmp) throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    HotswapAgentAttachment attachment = newAttachment(tmp).setOverridePath(local).build();

    List<String> arguments = attachment.getArguments();

    Path agentProperties = tmp.resolve("hotswap/hotswap-agent.properties");
    assertTrue(arguments.contains(JAVAAGENT_PREFIX + local.toAbsolutePath()
        + "=autoHotswap=true,LOGGER=error,propertiesFilePath=" + agentProperties.toAbsolutePath()));
    assertEquals(
        "autoHotswap=true\nLOGGER=error\nextraClasspath="
            + local.toAbsolutePath().toString().replace('\\', '/') + "\n",
        Files.readString(agentProperties),
        "the written file replaces the agent bundled configuration and names the agent jar as the"
            + " extra classpath every application classloader receives");
    assertTrue(arguments.contains(JAVAAGENT_PREFIX + observerJar(tmp).toAbsolutePath()),
        "the redefinition observer attaches behind the agent");
    assertTrue(arguments.containsAll(List.of("--add-opens=java.base/java.lang=ALL-UNNAMED",
        "--add-opens=java.base/java.io=ALL-UNNAMED", "--add-opens=java.base/java.net=ALL-UNNAMED",
        "--add-opens=java.base/jdk.internal.loader=ALL-UNNAMED",
        "--add-opens=java.desktop/java.beans=ALL-UNNAMED")),
        "the agent needs the reflective opens as self contained tokens");
    assertEquals(0, jarRequests.get(), "nothing is downloaded for an override");
  }

  @Test
  void shouldFailWithoutTheObserverJar(@TempDir Path tmp) throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    HotswapAgentAttachment attachment =
        HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
            .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
            .setOverridePath(local).setJavaExecutable(fakeJava(tmp, 0)).build();

    IOException failure = assertThrows(IOException.class, attachment::getArguments);

    assertTrue(failure.getMessage().contains(HotswapObserverJar.ARTIFACT_ID),
        "the missing requirement is named");
  }

  @Test
  void shouldFailWhenTheObserverJarDoesNotExist(@TempDir Path tmp) throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    HotswapAgentAttachment attachment = newAttachment(tmp).setOverridePath(local)
        .setObserverJar(tmp.resolve("missing-observer.jar")).build();

    IOException failure = assertThrows(IOException.class, attachment::getArguments);

    assertTrue(failure.getMessage().contains("missing-observer.jar"),
        "the missing location is named");
  }

  @Test
  void shouldFailWhenTheOverridePathDoesNotExist(@TempDir Path tmp) throws Exception {
    HotswapAgentAttachment attachment =
        newAttachment(tmp).setOverridePath(tmp.resolve("missing.jar")).build();

    assertThrows(IOException.class, attachment::getArguments);
  }

  @Test
  void shouldWarnAndAttachLimitedOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment =
        HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
            .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
            .setOverridePath(local).setObserverJar(observerJar(tmp))
            .setJavaExecutable(fakeJava(tmp, 1)).setWarn(warnings::add).build();

    List<String> arguments = attachment.getArguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX)),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch(REDEFINITION_ARGUMENT::equals),
        "the unsupported flag never reaches the virtual machine");
    assertTrue(arguments.contains(HotswapAgentAttachment.TOOL_ARGUMENT));
    assertTrue(arguments.contains(HotswapAgentAttachment.LEVEL_ARGUMENT_PREFIX + "limited"),
        "the application learns the limited depth of this run");
    String warning = String.join("\n", warnings);
    assertTrue(warning.contains("####"), "the warning frames itself so it cannot be missed");
    assertTrue(warning.contains("method body changes"));
    assertTrue(warning.contains(REDEFINITION_ARGUMENT), "the requirement is named for the user");
  }

  @Test
  void shouldWarnAndAttachLimitedWhenTheJavaExecutableIsMissing(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment =
        HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
            .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
            .setOverridePath(local).setObserverJar(observerJar(tmp))
            .setJavaExecutable(tmp.resolve("missing-java")).setWarn(warnings::add).build();

    List<String> arguments = attachment.getArguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX)));
    assertTrue(arguments.stream().noneMatch(REDEFINITION_ARGUMENT::equals));
    assertTrue(String.join("\n", warnings).contains("java executable"));
  }

  @Test
  void shouldAskTheRunningVirtualMachineWhenNoExecutableIsNamed(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment =
        HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
            .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
            .setOverridePath(local).setObserverJar(observerJar(tmp))
            .setRunningVirtualMachineOptions(HotswapAgentAttachment.REDEFINITION_OPTION::equals)
            .setWarn(warnings::add).build();

    List<String> arguments = attachment.getArguments();

    assertTrue(arguments.contains(REDEFINITION_ARGUMENT),
        "the running virtual machine answered the capability itself");
    assertTrue(arguments.contains(HotswapAgentAttachment.TOOL_ARGUMENT));
    assertTrue(arguments.contains(HotswapAgentAttachment.LEVEL_ARGUMENT_PREFIX + "full"),
        "the application learns the full depth of this run");
    assertTrue(warnings.isEmpty());
  }

  @Test
  void shouldWarnAndAttachLimitedWhenTheRunningVirtualMachineLacksTheOption(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment =
        HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
            .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
            .setOverridePath(local).setObserverJar(observerJar(tmp))
            .setRunningVirtualMachineOptions(option -> false).setWarn(warnings::add).build();

    List<String> arguments = attachment.getArguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX)),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch(REDEFINITION_ARGUMENT::equals));
    assertTrue(String.join("\n", warnings).contains("method body changes"));
  }

  @Test
  @SuppressWarnings("checkstyle:LineLength")
  void shouldWriteTheSharedContainerOptionsAndReturnTheArgument(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    Path java = fakeJava(tmp, 0);
    Path directory = tmp.resolve("shared");
    List<String> lines = new ArrayList<>();
    List<String> warnings = new ArrayList<>();
    HotswapAgentAttachment attachment = newAttachment(tmp).setOverridePath(local)
        .setJavaExecutable(java).setLog(lines::add).setWarn(warnings::add).build();

    List<String> arguments = attachment.getSharedContainerArguments(directory);

    String argument = "-XX:VMOptionsFile=" + directory.resolve(OPTIONS_FILE).toAbsolutePath();
    Path properties = directory.resolve("hotswap-agent.properties");
    assertEquals(List.of(argument), arguments);
    assertEquals("""
        autoHotswap=true
        LOGGER=error
        extraClasspath=%s
        excludedClassLoaderPatterns=org.eclipse.*
        """.formatted(local.toAbsolutePath().toString().replace('\\', '/')),
        Files.readString(properties));
    assertEquals(
        """
            -XX:+AllowEnhancedClassRedefinition
            -javaagent:%s=autoHotswap=true,LOGGER=error,disablePlugin=JacksonPlugin,propertiesFilePath=%s
            --add-opens=java.base/java.lang=ALL-UNNAMED
            --add-opens=java.base/java.io=ALL-UNNAMED
            --add-opens=java.base/java.net=ALL-UNNAMED
            --add-opens=java.base/jdk.internal.loader=ALL-UNNAMED
            --add-opens=java.desktop/java.beans=ALL-UNNAMED
            -javaagent:%s
            -Dwebforj.hotswap.tool=hotswapAgent
            -Dwebforj.hotswap.level=full
            """
            .formatted(local.toAbsolutePath(), properties.toAbsolutePath(),
                observerJar(tmp).toAbsolutePath()),
        Files.readString(directory.resolve(OPTIONS_FILE)));
    assertEquals(List.of(), lines);
    assertEquals(List.of(), warnings);
  }

  @Test
  @SuppressWarnings("checkstyle:LineLength")
  void shouldRewriteTheSharedContainerOptionsForTheLimitedLevel(@TempDir Path tmp)
      throws Exception {
    Path local = Files.write(tmp.resolve(AGENT_JAR), JAR_BYTES);
    Path directory = tmp.resolve("shared");
    List<String> lines = new ArrayList<>();
    newAttachment(tmp).setOverridePath(local).build().getSharedContainerArguments(directory);

    newAttachment(tmp).setOverridePath(local).setJavaExecutable(fakeJava(tmp, 1)).setLog(lines::add)
        .build().getSharedContainerArguments(directory);

    assertEquals(List.of(), lines);
    assertEquals(
        """
            -javaagent:%s=autoHotswap=true,LOGGER=error,disablePlugin=JacksonPlugin,propertiesFilePath=%s
            --add-opens=java.base/java.lang=ALL-UNNAMED
            --add-opens=java.base/java.io=ALL-UNNAMED
            --add-opens=java.base/java.net=ALL-UNNAMED
            --add-opens=java.base/jdk.internal.loader=ALL-UNNAMED
            --add-opens=java.desktop/java.beans=ALL-UNNAMED
            -javaagent:%s
            -Dwebforj.hotswap.tool=hotswapAgent
            -Dwebforj.hotswap.level=limited
            """
            .formatted(local.toAbsolutePath(),
                directory.resolve("hotswap-agent.properties").toAbsolutePath(),
                observerJar(tmp).toAbsolutePath()),
        Files.readString(directory.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldFallBackToTheDefaultVersion(@TempDir Path tmp) throws Exception {
    assertEquals(HotswapAgentAttachment.DEFAULT_VERSION,
        newAttachment(tmp).setVersion(" ").build().getVersion());
  }

  private HotswapAgentAttachment.Builder newAttachment(Path tmp) throws IOException {
    return HotswapAgentAttachment.create().setCacheRoot(tmp.resolve(CACHE_FOLDER))
        .setConfigurationDirectory(tmp.resolve(HOTSWAP_FOLDER)).setRepositoryHost(host)
        .setObserverJar(observerJar(tmp)).setJavaExecutable(fakeJava(tmp, 0));
  }

  private static Path observerJar(Path dir) throws IOException {
    Path jar = dir.resolve("webforj-hotswap-observer.jar");
    if (!Files.exists(jar)) {
      Files.createFile(jar);
    }

    return jar;
  }

  private static Path fakeJava(Path dir, int exitCode) throws IOException {
    Assumptions.assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"),
        "the capability check stand in needs a posix file system");

    Path script = dir.resolve("java-" + exitCode);
    Files.writeString(script, "#!/bin/sh\nexit " + exitCode + "\n");
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"));

    return script;
  }

  private static String sha1(byte[] bytes) {
    try {
      return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-1").digest(bytes)); // NOSONAR
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
