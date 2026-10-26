package com.webforj.testbed.interaction;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.component.appnav.AppNavInteraction;
import com.webforj.automation.act.component.appnav.AppNavItemInteraction;
import com.webforj.automation.act.component.infinitescroll.InfiniteScrollInteraction;
import com.webforj.automation.act.component.login.LoginInteraction;
import com.webforj.automation.act.component.navigator.NavigatorInteraction;
import com.webforj.automation.act.component.refresher.RefresherInteraction;
import com.webforj.automation.act.component.slider.SliderInteraction;
import com.webforj.automation.act.component.splitter.SplitterInteraction;
import com.webforj.automation.act.component.tabbedpane.TabbedPaneInteraction;
import com.webforj.automation.act.component.table.TableInteraction;
import com.webforj.automation.act.component.tree.TreeInteraction;
import com.webforj.automation.act.component.upload.UploadInteraction;
import com.webforj.component.event.ComponentEvent;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ScanResult;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import org.junit.jupiter.api.Test;

class InteractionCatalogGuardTest {
  private static final String COMPONENT_PACKAGE = "com.webforj.component";
  private static final Set<String> RAISED_BY_THE_RUNTIME =
      Set.of("com.webforj.component.element.event.ElementEvent",
          "com.webforj.component.element.event.ElementDefinedEvent",
          "com.webforj.component.event.ExecuteAsyncScriptEvent",
          "com.webforj.component.navigator.event.NavigatorChangeEvent");
  private static final Map<String, String> CAUSED_BY_THE_VERB = Map.ofEntries(
      Map.entry("AccordionPanelCloseEvent", "close"), Map.entry("AccordionPanelOpenEvent", "open"),
      Map.entry("AccordionPanelToggleEvent", "open"), Map.entry("AlertCloseEvent", "close"),
      Map.entry("AppNavLocationChangedEvent", "click"), Map.entry("BlurEvent", "focus"),
      Map.entry("ButtonClickEvent", "click"), Map.entry("CheckEvent", "check"),
      Map.entry("ElementClickEvent", "click"), Map.entry("FocusEvent", "focus"),
      Map.entry("GoogleChartSelectedEvent", "select"), Map.entry("KeypressEvent", "keypress"),
      Map.entry("ListClickEvent", "click"), Map.entry("ListCloseEvent", "close"),
      Map.entry("ListOpenEvent", "open"), Map.entry("ListSelectEvent", "select"),
      Map.entry("LoginCancelEvent", "cancel"), Map.entry("ModifyEvent", "type"),
      Map.entry("RadioButtonGroupChangeEvent", "check"), Map.entry("TabDeselectEvent", "select"),
      Map.entry("TabSelectEvent", "select"), Map.entry("TableCellClickEvent", "click"),
      Map.entry("TableCellDoubleClickEvent", "doubleClick"),
      Map.entry("TableItemClickEvent", "click"), Map.entry("TableItemDeselectEvent", "deselect"),
      Map.entry("TableItemDoubleClickEvent", "doubleClick"),
      Map.entry("TableItemSelectEvent", "select"), Map.entry("TableItemSelectionChange", "select"),
      Map.entry("TerminalDataEvent", "type"), Map.entry("ToggleEvent", "check"),
      Map.entry("TreeClickEvent", "click"), Map.entry("TreeDeselectEvent", "deselect"),
      Map.entry("TreeDoubleClickEvent", "doubleClick"), Map.entry("TreeSelectEvent", "select"),
      Map.entry("UncheckEvent", "uncheck"), Map.entry("UploadCancelEvent", "cancel"),
      Map.entry("WindowClickEvent", "click"));
  private static final Map<String, PerformedInteraction> CAUSED_BY_THE_INTERACTION = Map.ofEntries(
      Map.entry("AppNavPinEvent", new PerformedInteraction(AppNavItemInteraction.class, "pin")),
      Map.entry("AppNavSearchEvent", new PerformedInteraction(AppNavInteraction.class, "search")),
      Map.entry("InfiniteScrollEvent",
          new PerformedInteraction(InfiniteScrollInteraction.class, "scroll")),
      Map.entry("LoginSubmitEvent", new PerformedInteraction(LoginInteraction.class, "submit")),
      Map.entry("NavigatorMoveFirstEvent",
          new PerformedInteraction(NavigatorInteraction.class, "moveFirst")),
      Map.entry("NavigatorMoveLastEvent",
          new PerformedInteraction(NavigatorInteraction.class, "moveLast")),
      Map.entry("NavigatorMoveNextEvent",
          new PerformedInteraction(NavigatorInteraction.class, "moveNext")),
      Map.entry("NavigatorMovePreviousEvent",
          new PerformedInteraction(NavigatorInteraction.class, "movePrevious")),
      Map.entry("RefresherRefreshEvent",
          new PerformedInteraction(RefresherInteraction.class, "refresh")),
      Map.entry("SliderSlideEvent", new PerformedInteraction(SliderInteraction.class, "slide")),
      Map.entry("SplitterResizeEvent",
          new PerformedInteraction(SplitterInteraction.class, "resize")),
      Map.entry("TabCloseEvent", new PerformedInteraction(TabbedPaneInteraction.class, "close")),
      Map.entry("TableColumnMoveEvent",
          new PerformedInteraction(TableInteraction.class, "moveColumn")),
      Map.entry("TableColumnResizeEvent",
          new PerformedInteraction(TableInteraction.class, "resizeColumn")),
      Map.entry("TableSortChangeEvent", new PerformedInteraction(TableInteraction.class, "sort")),
      Map.entry("TreeCollapseEvent", new PerformedInteraction(TreeInteraction.class, "collapse")),
      Map.entry("TreeExpandEvent", new PerformedInteraction(TreeInteraction.class, "expand")),
      Map.entry("UploadEvent", new PerformedInteraction(UploadInteraction.class, "upload")),
      Map.entry("UploadRejectEvent", new PerformedInteraction(UploadInteraction.class, "upload")));
  private static final Set<String> FIRED_WITH_ITS_CONSTRUCTOR =
      Set.of("AppLayoutDrawerCloseEvent", "AppLayoutDrawerOpenEvent", "DialogCloseEvent",
          "DialogOpenEvent", "DrawerCloseEvent", "DrawerOpenEvent", "GoogleChartReadyEvent",
          "MouseEnterEvent", "MouseExitEvent", "RendererClickEvent", "RightMouseDownEvent",
          "TerminalKeyEvent", "UploadChangeEvent", "UploadCompleteEvent", "UploadErrorEvent",
          "UploadFilterChangeEvent", "UploadListProgressEvent", "UploadProgressEvent");

  @Test
  void shouldKnowHowTheTestReachesEveryComponentEvent() {
    List<Class<?>> events = discoverEvents();
    assertTrue(events.size() >= 70,
        "Expected the guard to discover the component events but found " + events.size());

    List<String> missing = new ArrayList<>();
    for (Class<?> event : events) {
      String name = event.getSimpleName();
      if (!CAUSED_BY_THE_VERB.containsKey(name) && !CAUSED_BY_THE_INTERACTION.containsKey(name)
          && !FIRED_WITH_ITS_CONSTRUCTOR.contains(name)) {
        missing.add(event.getName());
      }
    }

    if (!missing.isEmpty()) {
      fail("Every component event needs the verb of UserInteraction that causes it, the "
          + "interaction of its component that causes it, or a place among the events a test "
          + "fires with their constructor. Missing " + missing.size() + ":" + System.lineSeparator()
          + String.join(System.lineSeparator(), missing));
    }
  }

  @Test
  void shouldNameVerbsThatExist() {
    List<String> verbs = new ArrayList<>();
    for (Method method : UserInteraction.class.getMethods()) {
      if (Modifier.isStatic(method.getModifiers())) {
        verbs.add(method.getName());
      }
    }

    List<String> unknown = new ArrayList<>();
    for (Map.Entry<String, String> entry : CAUSED_BY_THE_VERB.entrySet()) {
      if (!verbs.contains(entry.getValue())) {
        unknown.add(entry.getKey() + " -> " + entry.getValue());
      }
    }

    assertTrue(unknown.isEmpty(),
        "The guard names verbs UserInteraction does not have: " + unknown);
  }

  @Test
  void shouldNameComponentInteractionsThatExist() {
    List<String> unknown = new ArrayList<>();
    for (Map.Entry<String, PerformedInteraction> entry : CAUSED_BY_THE_INTERACTION.entrySet()) {
      if (!entry.getValue().exists()) {
        unknown.add(entry.getKey() + " -> " + entry.getValue());
      }
    }

    assertTrue(unknown.isEmpty(),
        "The guard names component interactions that do not exist: " + unknown);
  }

  private static List<Class<?>> discoverEvents() {
    List<Class<?>> events = new ArrayList<>();
    try (ScanResult scan = new ClassGraph().enableClassInfo().ignoreClassVisibility()
        .acceptPackages(COMPONENT_PACKAGE).scan()) {
      for (ClassInfo classInfo : scan.getSubclasses(ComponentEvent.class.getName())) {
        Object element = classInfo.getClasspathElementFile();
        boolean shipped = element != null && element.toString().contains("webforj-");
        boolean base = !classInfo.getSubclasses().isEmpty();
        if (!shipped || base || !classInfo.isPublic() || classInfo.isAbstract()
            || classInfo.isInnerClass() || RAISED_BY_THE_RUNTIME.contains(classInfo.getName())) {
          continue;
        }

        Class<?> event = classInfo.loadClass();
        if (hasDataConstructor(event)) {
          events.add(event);
        }
      }
    }

    events.sort((left, right) -> left.getName().compareTo(right.getName()));

    return events;
  }

  private static boolean hasDataConstructor(Class<?> event) {
    for (Constructor<?> constructor : event.getConstructors()) {
      Class<?>[] parameters = constructor.getParameterTypes();
      if (parameters.length == 2 && parameters[1] == Map.class) {
        return true;
      }
    }

    return false;
  }

  private static final class PerformedInteraction {
    private final Class<?> type;
    private final String method;

    PerformedInteraction(Class<?> type, String method) {
      this.type = type;
      this.method = method;
    }

    boolean exists() {
      for (Method candidate : type.getMethods()) {
        if (candidate.getName().equals(method) && Modifier.isStatic(candidate.getModifiers())
            && candidate.getReturnType() == Consumer.class) {
          return true;
        }
      }

      return false;
    }

    @Override
    public String toString() {
      return type.getSimpleName() + "." + method;
    }
  }
}
