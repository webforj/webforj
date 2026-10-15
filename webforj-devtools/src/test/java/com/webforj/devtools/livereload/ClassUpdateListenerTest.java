package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.devtools.livereload.fixture.ArchiveView;
import com.webforj.devtools.livereload.fixture.ClassFolderLoader;
import com.webforj.devtools.livereload.fixture.DashboardCard;
import com.webforj.devtools.livereload.fixture.DashboardView;
import com.webforj.devtools.livereload.fixture.DefaultPackageApp;
import com.webforj.devtools.livereload.fixture.ForeignRoutesApp;
import com.webforj.devtools.livereload.fixture.GhostClassLoader;
import com.webforj.devtools.livereload.fixture.InboxView;
import com.webforj.devtools.livereload.fixture.LayoutBadge;
import com.webforj.devtools.livereload.fixture.MainLayoutView;
import com.webforj.devtools.livereload.fixture.OtherCard;
import com.webforj.devtools.livereload.fixture.OtherView;
import com.webforj.devtools.livereload.fixture.ReportsChartView;
import com.webforj.devtools.livereload.fixture.ReportsView;
import com.webforj.devtools.livereload.fixture.RoutedApp;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import com.webforj.router.history.MemoryHistory;
import com.webforj.router.observer.RouteRendererObserver;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

class ClassUpdateListenerTest {

  private static final String UNREADABLE = "unreadable";
  private static final String CLASS_FILE_SUFFIX = ".class";

  private ClassUpdateListener listener;
  private Router router;
  private Page page;
  private MockedStatic<ConceiverProvider> mockedConceiverProvider;
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Page> mockedPage;

  @BeforeEach
  void setUp() throws BBjException {
    mockedConceiverProvider = mockStatic(ConceiverProvider.class);
    when(ConceiverProvider.getCurrent()).thenReturn(new DefaultConceiver());

    RouteRegistry registry = new RouteRegistry();
    registry.register("/layout", MainLayoutView.class);
    registry.register("/layout/dashboard", DashboardView.class, MainLayoutView.class);
    registry.register("/other", OtherView.class);
    router = new Router(registry, new MemoryHistory());

    page = mock(Page.class);
    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);

    mockedEnvironment = mockStatic(Environment.class);
    Environment current = mock(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(current);
    BBjSysGui sysGui = mock(BBjSysGui.class);
    when(Environment.getCurrent().getSysGui()).thenReturn(sysGui);

    Frame frame = mock(Frame.class);
    when(frame.getName()).thenReturn("frame1");
    BBjTopLevelWindow window = mock(BBjTopLevelWindow.class);
    when(window.getUserData()).thenReturn(frame);
    when(sysGui.getWindows()).thenReturn(new BBjVector(List.of(window)));

    listener = new ClassUpdateListener();
  }

  @AfterEach
  void tearDown() {
    mockedEnvironment.close();
    mockedPage.close();
    mockedConceiverProvider.close();
  }

  @Test
  void shouldRebuildTheRenderedViewInPlace() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);
    final Component layout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    assertTrue(firstView.isDestroyed());
    assertNotSame(firstView, renderedComponent(DashboardView.class));
    assertFalse(layout.isDestroyed());
    assertSame(layout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildFromTheTopmostAffectedNode() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);
    Component firstLayout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(DashboardView.class.getName(), MainLayoutView.class.getName()), router,
        page);

    assertTrue(firstLayout.isDestroyed());
    assertTrue(firstView.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    assertNotSame(firstView, renderedComponent(DashboardView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildOnlyTheViewThatReachesTheChangedClass() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);
    final Component layout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(DashboardCard.class.getName()), router, page);

    assertTrue(firstView.isDestroyed());
    assertNotSame(firstView, renderedComponent(DashboardView.class));
    assertSame(layout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildFromTheLayoutThatReachesTheChangedClass() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstLayout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(LayoutBadge.class.getName(), DashboardCard.class.getName()), router,
        page);

    assertTrue(firstLayout.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildFromTheChangedLayoutThatAloneReachesTheOtherChangedClass() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstLayout = renderedComponent(MainLayoutView.class);

    listener.apply(Set.of(MainLayoutView.class.getName(), LayoutBadge.class.getName()), router,
        page);

    assertTrue(firstLayout.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldReloadForTheClassOnlyAnUnrenderedRouteReaches() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of(OtherCard.class.getName()), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldReloadForTheClassOutsideTheRouteTree() {
    // Nothing is guessed about a class the route tree cannot account for, such as a service.
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of("com.example.InvoiceService"), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
    assertSame(firstView, renderedComponent(DashboardView.class));
  }

  @Test
  void shouldReloadWhenTheChangeTouchesTheTreeAndSomethingOutsideIt() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of(DashboardView.class.getName(), "com.example.InvoiceService"), router,
        page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldDoNothingWhenTheChangedRouteIsNotRenderedHere() {
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    listener.apply(Set.of(OtherView.class.getName()), router, page);

    verify(page, never()).reload();
    assertFalse(firstView.isDestroyed());
    assertSame(firstView, renderedComponent(DashboardView.class));
  }

  @Test
  void shouldReloadWhenTheRebuildIsVetoed() {
    router.navigate(new Location("/layout/dashboard"));
    final Component firstView = renderedComponent(DashboardView.class);

    router.getRenderer().addObserver((component, event, context, cb) -> {
      if (component instanceof DashboardView
          && event == RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY) {
        cb.accept(false);
      } else {
        cb.accept(true);
      }
    });

    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldReloadThePageWhenTheRebuildThrows() {
    router.navigate(new Location("/layout/dashboard"));
    Router failing = spy(router);
    doThrow(new IllegalArgumentException("not registered")).when(failing)
        .navigate(any(Location.class), any(NavigationOptions.class), any());

    listener.apply(Set.of(DashboardView.class.getName()), failing, page);

    verify(page).reload();
  }

  @Test
  void shouldReloadWhenTheClassReferencesCannotBeRead() {
    ClassReferenceIndex failing = mock(ClassReferenceIndex.class);
    ClassReferenceIndex.Update update = mock(ClassReferenceIndex.Update.class);
    when(failing.newUpdate(any(), any())).thenReturn(update);
    when(update.newWalk(any())).thenThrow(new UncheckedIOException(new IOException(UNREADABLE)));
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    new ClassUpdateListener(failing).apply(Set.of(DashboardCard.class.getName()), router, page);

    verify(page).reload();
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldRebuildTheChangedRouteWithoutReadingAnyReferences() {
    ClassReferenceIndex index = mock(ClassReferenceIndex.class);
    ClassReferenceIndex.Update update = mock(ClassReferenceIndex.Update.class);
    ClassReferenceIndex.Walk walk = mock(ClassReferenceIndex.Walk.class);
    when(index.newUpdate(any(), any())).thenReturn(update);
    when(update.newWalk(any())).thenReturn(walk);
    when(walk.isComplete()).thenReturn(true);
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    new ClassUpdateListener(index).apply(Set.of(DashboardView.class.getName()), router, page);

    assertTrue(firstView.isDestroyed());
    verify(walk, never()).reach(any());
    verify(page, never()).reload();
  }

  @Test
  void shouldReloadWhenTheApplicationHasNoRouter() {
    listener.apply(Set.of(DashboardView.class.getName()), null, page);

    verify(page).reload();
  }

  @Test
  void shouldReloadWhenNoRouteIsRendered() {
    listener.apply(Set.of(DashboardView.class.getName()), router, page);

    verify(page).reload();
  }

  @Test
  void shouldRegisterTheNewRouteTheChangedLayoutReferencesBeforeTheRebuild() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    final Component firstLayout = renderedComponent(MainLayoutView.class);

    discovering.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertEquals(1, scans.size());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isPresent());
    assertTrue(firstLayout.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldUnregisterTheRouteWhoseClassFileIsGoneBeforeTheRebuild() throws Exception {
    Class<? extends Component> gone = loadGhost(InboxView.class);
    router.getRegistry().register("/layout/inbox", gone, MainLayoutView.class);
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    final Component firstLayout = renderedComponent(MainLayoutView.class);

    listening.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertTrue(router.getRegistry().getRouteByComponent(gone).isEmpty());
    assertTrue(firstLayout.isDestroyed());
    verify(page, never()).reload();
  }

  @Test
  void shouldRemoveTheRouteWhosePathHasSurroundingSpaces() throws Exception {
    Class<? extends Component> gone = loadGhost(InboxView.class);
    router.getRegistry().register(" inbox ", gone, MainLayoutView.class);
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    Set<String> changed = Set.of(OtherView.class.getName());

    assertTimeoutPreemptively(Duration.ofSeconds(5), () -> listening.apply(changed, router, page));

    assertTrue(router.getRegistry().getAvailableRouteEntires().stream()
        .noneMatch(entry -> entry.getComponent() == gone));
  }

  @Test
  void shouldKeepTheOtherRouteOnTheSamePath() throws Exception {
    Class<? extends Component> gone = loadGhost(InboxView.class);
    router.getRegistry().register(new RouteEntry("/other", gone, Frame.class, null, 5));
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    listening.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertTrue(router.getRegistry().getRouteByComponent(gone).isEmpty());
    assertEquals(Optional.of("/other"), router.getRegistry().getRouteByComponent(OtherView.class));
  }

  @Test
  void shouldKeepTheRoutesWhoseClassFilesExist() {
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    listening.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertEquals(3, router.getRegistry().getAvailableRouteEntires().size());
    verify(page, never()).reload();
  }

  @Test
  void shouldKeepTheRoutesOutsideTheRoutePackagesWithoutClassFile() throws Exception {
    Class<? extends Component> gone = loadGhost(InboxView.class);
    router.getRegistry().register("/layout/inbox", gone, MainLayoutView.class);
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new ForeignRoutesApp());
    router.navigate(new Location("/layout/dashboard"));

    listening.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertTrue(router.getRegistry().getRouteByComponent(gone).isPresent());
  }

  @Test
  void shouldRegisterTheChangedNewRoute() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    final Component firstView = renderedComponent(DashboardView.class);

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertEquals(1, scans.size());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isPresent());
    assertFalse(firstView.isDestroyed());
    verify(page, never()).reload();
  }

  @Test
  void shouldNotDiscoverRoutesForTheReferencedRouteOutsideTheRoutePackages() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new ForeignRoutesApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertTrue(scans.isEmpty());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isEmpty());
  }

  @Test
  void shouldDiscoverRoutesForTheChangedClassWhoseReferencesCannotBeRead() {
    ClassReferenceIndex failing = mock(ClassReferenceIndex.class);
    ClassReferenceIndex.Update update = mock(ClassReferenceIndex.Update.class);
    when(failing.newUpdate(any(), any())).thenReturn(update);
    when(update.isRoute(any())).thenThrow(new UncheckedIOException(new IOException(UNREADABLE)));
    when(update.getReferences(any()))
        .thenThrow(new UncheckedIOException(new IOException(UNREADABLE)));
    when(update.newWalk(any())).thenThrow(new UncheckedIOException(new IOException(UNREADABLE)));
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering = new ClassUpdateListener(failing, packages -> {
      scans.add(packages);
      return new RouteRegistry().register(InboxView.class);
    });
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertEquals(1, scans.size());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isPresent());
  }

  @Test
  void shouldNotDiscoverRoutesForTheChangedRouteTheRegistryKnows() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    Component firstView = renderedComponent(DashboardView.class);

    discovering.apply(Set.of(DashboardView.class.getName()), router, page);

    assertTrue(scans.isEmpty());
    assertTrue(firstView.isDestroyed());
    verify(page, never()).reload();
  }

  @Test
  void shouldNotDiscoverRoutesForTheChangedClassThatIsNoRoute() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(DashboardCard.class.getName()), router, page);

    assertTrue(scans.isEmpty());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isEmpty());
    verify(page, never()).reload();
  }

  @Test
  void shouldNotDiscoverRoutesForTheChangedClassThatCannotBeLoaded() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of("com.webforj.devtools.livereload.fixture.MissingView"), router, page);

    assertTrue(scans.isEmpty());
    verify(page).reload();
  }

  @Test
  void shouldNotDiscoverRoutesBeforeTheApplicationRuns() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertTrue(scans.isEmpty());
    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isEmpty());
  }

  @Test
  void shouldDiscoverRoutesInThePackagesTheApplicationNames() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertEquals(1, scans.size());
    assertArrayEquals(new String[] {"com.webforj.devtools.livereload.fixture", "org.example.views"},
        scans.get(0));
  }

  @Test
  void shouldDiscoverRoutesInThePackageOfTheApplication() {
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering =
        createDiscoveringListener(scans, new RouteRegistry().register(InboxView.class));
    discovering.onDidRun(new DefaultPackageApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertEquals(1, scans.size());
    assertArrayEquals(new String[] {DefaultPackageApp.class.getPackageName()}, scans.get(0));
  }

  @Test
  void shouldNotRegisterTheDiscoveredRoutesTheRegistryKnows() {
    RouteRegistry discovered = new RouteRegistry();
    discovered.register("/layout", MainLayoutView.class);
    discovered.register("/other", OtherView.class);
    discovered.register(InboxView.class);
    ClassUpdateListener discovering = createDiscoveringListener(new ArrayList<>(), discovered);
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    List<Class<? extends Component>> registered = router.getRegistry().getAvailableRouteEntires()
        .stream().map(RouteEntry::getComponent).toList();
    assertEquals(4, registered.size());
    assertEquals(
        Set.of(MainLayoutView.class, DashboardView.class, OtherView.class, InboxView.class),
        Set.copyOf(registered));
  }

  @Test
  void shouldReadEachChangedClassFileOnce() throws URISyntaxException, IOException {
    ClassReferenceIndex index = spy(new ClassReferenceIndex());
    ClassUpdateListener reading = new ClassUpdateListener(index, packages -> new RouteRegistry());
    reading.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    reading.apply(Set.of(LayoutBadge.class.getName(), DashboardCard.class.getName()), router, page);

    verify(index)
        .readClassFile(Path.of(LayoutBadge.class.getResource("LayoutBadge.class").toURI()));
    verify(page, never()).reload();
  }

  @Test
  void shouldFindTheNewRouteWithoutLoadingItsClass() {
    String unloadable = "com.webforj.devtools.livereload.fixture.UnloadableView";
    ClassReferenceIndex index = mock(ClassReferenceIndex.class);
    ClassReferenceIndex.Update update = mock(ClassReferenceIndex.Update.class);
    ClassReferenceIndex.Walk walk = mock(ClassReferenceIndex.Walk.class);
    when(index.newUpdate(any(), any())).thenReturn(update);
    when(update.getReferences(MainLayoutView.class.getName())).thenReturn(Set.of(unloadable));
    when(update.isRoute(unloadable)).thenReturn(true);
    when(update.newWalk(any())).thenReturn(walk);
    when(walk.isComplete()).thenReturn(true);
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering = new ClassUpdateListener(index, packages -> {
      scans.add(packages);
      return new RouteRegistry();
    });
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(MainLayoutView.class.getName()), router, page);

    assertEquals(1, scans.size());
  }

  @Test
  void shouldRegisterOnlyTheNewRoutes() {
    RouteRegistry discovered = new RouteRegistry().register(InboxView.class);
    discovered.register(ArchiveView.class);
    ClassUpdateListener discovering = createDiscoveringListener(new ArrayList<>(), discovered);
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isPresent());
    assertTrue(router.getRegistry().getRouteByComponent(ArchiveView.class).isEmpty());
  }

  @Test
  void shouldKeepTheRouteTheAppUnregistered() {
    router.getRegistry().unregister(OtherView.class);
    RouteRegistry discovered = new RouteRegistry().register(InboxView.class);
    discovered.register("/other", OtherView.class);
    ClassUpdateListener discovering = createDiscoveringListener(new ArrayList<>(), discovered);
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertTrue(router.getRegistry().getRouteByComponent(InboxView.class).isPresent());
    assertTrue(router.getRegistry().getRouteByComponent(OtherView.class).isEmpty());
  }

  @Test
  void shouldResolveTheClassFileOfTheRegisteredRouteOnce(@TempDir Path folder) throws Exception {
    Path classFile =
        folder.resolve(InboxView.class.getName().replace('.', '/') + CLASS_FILE_SUFFIX);
    Files.createDirectories(classFile.getParent());
    Files.write(classFile, readFixtureClassFile(InboxView.class));
    ClassFolderLoader loader = new ClassFolderLoader(InboxView.class.getName(), folder);
    Class<? extends Component> route = loadFrom(loader, InboxView.class);
    router.getRegistry().register(route);
    ClassUpdateListener listening =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    listening.apply(Set.of(DashboardCard.class.getName()), router, page);
    listening.apply(Set.of(DashboardCard.class.getName()), router, page);

    assertEquals(1, loader.getLookups());
    assertTrue(router.getRegistry().getRouteByComponent(route).isPresent());
  }

  @Test
  void shouldBringBackTheRouteWhoseClassFileReturned(@TempDir Path folder) throws Exception {
    Path classFile =
        folder.resolve(InboxView.class.getName().replace('.', '/') + CLASS_FILE_SUFFIX);
    Files.createDirectories(classFile.getParent());
    byte[] bytes = readFixtureClassFile(InboxView.class);
    Files.write(classFile, bytes);
    Class<? extends Component> route =
        loadFrom(new ClassFolderLoader(InboxView.class.getName(), folder), InboxView.class);
    router.getRegistry().register(route);
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener listening =
        createDiscoveringListener(scans, new RouteRegistry().register(route));
    listening.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    listening.apply(Set.of(DashboardCard.class.getName()), router, page);

    Files.delete(classFile);
    listening.apply(Set.of(DashboardCard.class.getName()), router, page);
    final boolean goneWhileDeleted = router.getRegistry().getRouteByComponent(route).isEmpty();

    Files.write(classFile, bytes);
    listening.apply(Set.of(DashboardCard.class.getName()), router, page);

    assertTrue(goneWhileDeleted);
    assertEquals(1, scans.size());
    assertTrue(router.getRegistry().getRouteByComponent(route).isPresent());
    verify(page, never()).reload();
  }

  @Test
  void shouldRebuildTheNodeReferencingTheRouteWhosePathChanged() {
    router.getRegistry().register("/old-inbox", InboxView.class);
    ClassUpdateListener discovering =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    Component firstLayout = renderedComponent(MainLayoutView.class);

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertTrue(firstLayout.isDestroyed());
    assertNotSame(firstLayout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldKeepTheRouteAsBoundaryWhenItsPathIsUnchanged() {
    router.getRegistry().register(InboxView.class);
    ClassUpdateListener discovering =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    Component firstLayout = renderedComponent(MainLayoutView.class);

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertFalse(firstLayout.isDestroyed());
    assertSame(firstLayout, renderedComponent(MainLayoutView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldMoveTheChildRoutesWithTheirMovedOutlet() {
    router.getRegistry().register("old-reports", ReportsView.class);
    router.getRegistry().register(
        new RouteEntry("old-reports/chart", ReportsChartView.class, ReportsView.class, null, 10));
    ClassUpdateListener discovering =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(ReportsView.class.getName()), router, page);

    assertEquals(
        new RouteRegistry().register(ReportsChartView.class)
            .getRouteByComponent(ReportsChartView.class),
        router.getRegistry().getRouteByComponent(ReportsChartView.class));
    verify(page, never()).reload();
  }

  @Test
  void shouldNotReloadForTheMovedRouteThePageDoesNotShow() {
    router.getRegistry().register("old-reports", ReportsView.class);
    ClassUpdateListener discovering =
        createDiscoveringListener(new ArrayList<>(), new RouteRegistry());
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    final Component firstLayout = renderedComponent(MainLayoutView.class);
    final Component firstView = renderedComponent(DashboardView.class);

    discovering.apply(Set.of(ReportsView.class.getName()), router, page);

    verify(page, never()).reload();
    assertFalse(firstLayout.isDestroyed());
    assertFalse(firstView.isDestroyed());
  }

  @Test
  void shouldReapplyTheChangedPathOfTheRegisteredRoute() {
    router.getRegistry().register("/old-inbox", InboxView.class);
    List<String[]> scans = new ArrayList<>();
    ClassUpdateListener discovering = createDiscoveringListener(scans, new RouteRegistry());
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));

    discovering.apply(Set.of(InboxView.class.getName()), router, page);

    assertEquals(
        new RouteRegistry().register(InboxView.class).getAvailableRouteEntires().stream()
            .map(RouteEntry::getPath).toList(),
        router.getRegistry().getAvailableRouteEntires().stream()
            .filter(entry -> entry.getComponent() == InboxView.class).map(RouteEntry::getPath)
            .toList());
    assertTrue(scans.isEmpty());
    verify(page, never()).reload();
  }

  @ParameterizedTest
  @MethodSource("getDiscoveryFailures")
  void shouldReloadThePageWhenTheDiscoveryFails(Function<String[], RouteRegistry> discovery) {
    ClassUpdateListener discovering = new ClassUpdateListener(new ClassReferenceIndex(), discovery);
    discovering.onDidRun(new RoutedApp());
    router.navigate(new Location("/layout/dashboard"));
    Component firstLayout = renderedComponent(MainLayoutView.class);

    discovering.apply(Set.of(MainLayoutView.class.getName()), router, page);

    verify(page).reload();
    assertFalse(firstLayout.isDestroyed());
  }

  @Test
  void shouldRegisterTheListenerWhenLiveReloadIsOn() {
    listener.register(new LiveReloadOptions().setEnabled(true), page);

    verify(page).addEventListener(eq(ClassUpdateListener.EVENT_TYPE), any(),
        any(PageEventOptions.class));
  }

  @Test
  void shouldNotRegisterWhenLiveReloadIsOff() {
    listener.register(new LiveReloadOptions(), page);

    verify(page, never()).addEventListener(any(), any(), any(PageEventOptions.class));
  }

  @Test
  void shouldHandTheParsedClassNamesOn() {
    ClassUpdateListener spied = spy(listener);
    doNothing().when(spied).apply(any(), any(), any());

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(
        Map.of(ClassUpdateListener.DATA_KEY, "[\"" + DashboardView.class.getName() + "\"]"));

    try (MockedStatic<Router> mockedRouter = mockStatic(Router.class)) {
      mockedRouter.when(Router::getCurrent).thenReturn(router);

      spied.handleUpdate(event);
    }

    verify(spied).apply(Set.of(DashboardView.class.getName()), router, page);
  }

  @Test
  void shouldIgnoreTheUnreadableUpdate() {
    ClassUpdateListener spied = spy(listener);

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of(ClassUpdateListener.DATA_KEY, "{broken"));

    assertDoesNotThrow(() -> spied.handleUpdate(event));
    verify(spied, never()).apply(any(), any(), any());
  }

  @Test
  void shouldIgnoreTheUpdateWithoutClassNames() {
    ClassUpdateListener spied = spy(listener);

    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of());

    assertDoesNotThrow(() -> spied.handleUpdate(event));
    verify(spied, never()).apply(any(), any(), any());
  }

  private static Stream<Arguments> getDiscoveryFailures() {
    Function<String[], RouteRegistry> rejecting = packages -> {
      throw new IllegalStateException("no component");
    };
    Function<String[], RouteRegistry> unlinked = packages -> {
      throw new NoClassDefFoundError("com/example/Missing");
    };

    return Stream.of(Arguments.of(rejecting), Arguments.of(unlinked));
  }

  private static byte[] readFixtureClassFile(Class<?> fixture) throws IOException {
    try (InputStream input =
        fixture.getResourceAsStream(fixture.getSimpleName() + CLASS_FILE_SUFFIX)) {
      return input.readAllBytes();
    }
  }

  @SuppressWarnings("unchecked")
  private static Class<? extends Component> loadFrom(ClassLoader loader,
      Class<? extends Component> original) throws ClassNotFoundException {
    return (Class<? extends Component>) Class.forName(original.getName(), false, loader);
  }

  @SuppressWarnings("unchecked")
  private static Class<? extends Component> loadGhost(Class<? extends Component> original)
      throws ClassNotFoundException {
    return (Class<? extends Component>) Class.forName(original.getName(), true,
        new GhostClassLoader(original.getName()));
  }

  private ClassUpdateListener createDiscoveringListener(List<String[]> scans,
      RouteRegistry discovered) {
    return new ClassUpdateListener(new ClassReferenceIndex(), packages -> {
      scans.add(packages);
      return discovered;
    });
  }

  private Component renderedComponent(Class<? extends Component> componentClass) {
    return router.getRenderer().getRenderedComponent(componentClass).orElseThrow();
  }
}
