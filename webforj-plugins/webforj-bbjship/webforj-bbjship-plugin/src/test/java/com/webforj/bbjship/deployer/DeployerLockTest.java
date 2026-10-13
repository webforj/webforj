package com.webforj.bbjship.deployer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminAppDeploymentApplication;
import com.basis.api.admin.BBjAdminAppDeploymentConfiguration;
import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.ClasspathRelease;
import com.webforj.bbjship.Deployer;
import com.webforj.bbjship.Deployer.DeploymentResult;
import com.webforj.bbjship.DeploymentException;
import com.webforj.bbjship.foundation.HelloApp;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

final class DeployerLockTest {

  private static final String NAME = "hello-app";
  private static final String LOCK_FILE = ".lock";

  @TempDir
  Path temporary;

  private byte[] archive;
  private Path root;
  private BBjAdminBase admin;
  private BBjAdminAppDeploymentConfiguration configuration;
  private AtomicInteger readings;
  private CountDownLatch entered;
  private CountDownLatch released;

  @BeforeEach
  void initServerThatHoldsTheFirstDeploymentInsideTheAdministrationSession() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(NAME));
    archive = Files.readAllBytes(HelloApp.writeArchive(app, temporary.resolve("hello-app.zip")));
    root = temporary.resolve("deployments");
    admin = mock(BBjAdminBase.class);
    configuration = mock(BBjAdminAppDeploymentConfiguration.class);
    readings = new AtomicInteger();
    entered = new CountDownLatch(1);
    released = new CountDownLatch(1);
    when(admin.getRemoteConfiguration()).thenAnswer(invocation -> {
      if (readings.incrementAndGet() == 1) {
        entered.countDown();
        released.await();
      }

      return configuration;
    });
    when(admin.getClasspathNames()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    when(configuration.getApplications()).thenReturn(new BBjAdminAdapterList<>(List.of()));
    BBjAdminAppDeploymentApplication created = mock(BBjAdminAppDeploymentApplication.class);
    when(configuration.createApplication()).thenReturn(created);
  }

  @Test
  void shouldHoldTheLockFileOfTheDeploymentsFolderDuringTheDeployment() throws Exception {
    InputStream upload = new ByteArrayInputStream(archive);
    String destination = root.toString();
    ExecutorService executor = Executors.newSingleThreadExecutor();

    try (executor) {
      Future<DeploymentResult> deployment = executor
          .submit(() -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));
      entered.await();
      try (FileChannel channel =
          FileChannel.open(root.resolve(LOCK_FILE), StandardOpenOption.WRITE)) {
        assertThrows(OverlappingFileLockException.class, channel::tryLock);
      } finally {
        released.countDown();
      }

      assertEquals(DeploymentResult.CREATED, deployment.get());
    }

    try (FileChannel channel = FileChannel.open(root.resolve(LOCK_FILE), StandardOpenOption.WRITE);
        FileLock lock = channel.tryLock()) {
      assertNotNull(lock);
    }
  }

  @Test
  void shouldWaitForTheLockHeldByAnotherLoaderOfTheProcess() throws Exception {
    released.countDown();
    InputStream upload = new ByteArrayInputStream(archive);
    String destination = root.toString();
    FutureTask<DeploymentResult> deployment = new FutureTask<>(
        () -> Deployer.deploy(upload, NAME, destination, admin, ClasspathRelease.NONE));
    Thread deploymentThread = new Thread(deployment);
    Files.createDirectories(root);

    try (FileChannel channel = FileChannel.open(root.resolve(LOCK_FILE), StandardOpenOption.CREATE,
        StandardOpenOption.WRITE)) {
      channel.lock();
      deploymentThread.start();
      while (deploymentThread.isAlive()
          && deploymentThread.getState() != Thread.State.TIMED_WAITING) {
        Thread.onSpinWait();
      }

      assertEquals(Thread.State.TIMED_WAITING, deploymentThread.getState());
      assertEquals(0, readings.get());
    }

    assertEquals(DeploymentResult.CREATED, deployment.get());
    assertEquals(1, readings.get());
  }

  @Test
  void shouldRunTheSecondDeploymentOfTheProcessAfterTheFirst() throws Exception {
    InputStream first = new ByteArrayInputStream(archive);
    InputStream second = new ByteArrayInputStream(archive);
    String destination = root.toString();
    FutureTask<DeploymentResult> blocked = new FutureTask<>(
        () -> Deployer.deploy(first, NAME, destination, admin, ClasspathRelease.NONE));
    FutureTask<DeploymentResult> waiting = new FutureTask<>(
        () -> Deployer.deploy(second, "second-app", destination, admin, ClasspathRelease.NONE));
    Thread waitingThread = new Thread(waiting);

    new Thread(blocked).start();
    entered.await();
    waitingThread.start();
    while (waitingThread.getState() != Thread.State.BLOCKED) {
      Thread.onSpinWait();
    }

    assertEquals(1, readings.get());

    released.countDown();

    assertEquals(DeploymentResult.CREATED, blocked.get());
    assertEquals(DeploymentResult.CREATED, waiting.get());
    assertEquals(2, readings.get());
  }

  @Test
  void shouldRefuseTheDevelopmentRunThatWaitedForTheArchiveDeploymentOfTheSameName()
      throws Exception {
    Path classFolder = temporary.resolve("hello-app/src/main/resources");
    Path development =
        HelloApp.writeArchive(temporary.resolve(NAME), temporary.resolve("development.zip"));
    try (FileSystem zip = FileSystems.newFileSystem(development)) {
      Files.delete(zip.getPath("application.jar"));
      Files.writeString(zip.getPath("development.properties"), """
          classFolder=%s
          """.formatted(classFolder.toString().replace("\\", "\\\\")));
    }

    Files.createFile(temporary.resolve("development.enabled"));
    BBjAdminAppDeploymentApplication published = mock(BBjAdminAppDeploymentApplication.class);
    Map<String, String> settings = new HashMap<>();
    AtomicBoolean committed = new AtomicBoolean();
    doAnswer(invocation -> settings.put(invocation.getArgument(0), invocation.getArgument(1)))
        .when(published).setString(anyString(), anyString());
    when(published.getString(anyString()))
        .thenAnswer(invocation -> settings.get(invocation.getArgument(0)));
    doAnswer(invocation -> {
      committed.set(true);
      return null;
    }).when(published).commit();
    when(configuration.createApplication()).thenReturn(published);
    when(configuration.getApplications()).thenAnswer(
        invocation -> new BBjAdminAdapterList<>(committed.get() ? List.of(published) : List.of()));
    InputStream first = new ByteArrayInputStream(archive);
    InputStream second = new ByteArrayInputStream(Files.readAllBytes(development));
    String destination = root.toString();
    FutureTask<DeploymentResult> blocked = new FutureTask<>(
        () -> Deployer.deploy(first, NAME, destination, admin, ClasspathRelease.NONE));
    FutureTask<DeploymentResult> waiting = new FutureTask<>(
        () -> Deployer.deploy(second, NAME, destination, admin, ClasspathRelease.NONE));
    Thread waitingThread = new Thread(waiting);

    new Thread(blocked).start();
    entered.await();
    waitingThread.start();
    while (waitingThread.getState() != Thread.State.BLOCKED) {
      Thread.onSpinWait();
    }

    released.countDown();

    assertEquals(DeploymentResult.CREATED, blocked.get());
    ExecutionException failure = assertThrows(ExecutionException.class, waiting::get);
    DeploymentException refusal = assertInstanceOf(DeploymentException.class, failure.getCause());
    assertEquals("hello-app is deployed from an archive. Undeploy it first", refusal.getMessage());
    Path current = root.resolve(NAME).resolve(Files.readString(root.resolve("hello-app/current")));
    try (Stream<Path> directories = Files.list(root.resolve(NAME))) {
      assertEquals(List.of(current), directories.filter(Files::isDirectory).toList());
    }

    assertFalse(Files.exists(current.resolve("development.properties")));
  }
}
