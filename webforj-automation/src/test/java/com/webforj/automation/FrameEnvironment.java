package com.webforj.automation;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.SysGuiProxyConstants;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxies.sysgui.BBjListBox;
import com.basis.bbj.proxies.sysgui.BBjNavigator;
import com.basis.bbj.proxies.sysgui.BBjTopLevelWindow;
import com.basis.bbj.proxies.sysgui.BBjTree;
import com.basis.bbj.proxies.sysgui.BBjWebComponent;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.basis.util.common.BasisNumber;
import com.webforj.Environment;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.conceiver.DefaultConceiver;
import com.webforj.router.Router;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/** The mocked environment a frame named Orders and the components added to it need in a test. */
public final class FrameEnvironment {

  private FrameEnvironment() {}

  /** Creates the environment, with the given router in its object table or none. */
  public static Environment create(Router router) throws Exception {
    Environment environment = mock(Environment.class);
    BBjSysGui sysGui = mock(BBjSysGui.class);
    BBjTopLevelWindow window = mock(BBjTopLevelWindow.class);
    BBjAPI api = mock(BBjAPI.class, RETURNS_DEEP_STUBS);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getBBjAPI()).thenReturn(api);
    when(api.getObjectTable().get(anyString())).thenReturn(null);
    when(api.getObjectTable().get(Router.class.getName())).thenReturn(router);
    when(api.getObjectTable().get(ConceiverProvider.LOOKUP_KEY)).thenReturn(new DefaultConceiver());
    when(sysGui.addWindow(any(), anyString(), any(byte[].class))).thenReturn(window);
    when(window.getText()).thenReturn("Orders");
    when(window.isVisible()).thenReturn(true);
    when(window.addTree(anyInt(), any(byte[].class))).thenAnswer(invocation -> createTree());
    when(window.addListBox(anyInt(), anyString(), any(byte[].class)))
        .thenAnswer(invocation -> createListBox());
    when(window.addNavigator(anyInt(), anyString(), any(byte[].class)))
        .thenAnswer(invocation -> createNavigator());
    when(window.addWebComponent(anyInt(), anyString(), any(byte[].class)))
        .thenAnswer(invocation -> createWebComponent());

    return environment;
  }

  private static BBjTree createTree() throws BBjException {
    BBjTree control = mock(BBjTree.class);
    Set<Integer> expanded = new HashSet<>();
    List<Integer> selected = new ArrayList<>();
    AtomicBoolean single = new AtomicBoolean();
    when(control.isVisible()).thenReturn(true);
    when(control.isEnabled()).thenReturn(true);
    doAnswer(invocation -> {
      single.set(invocation.getArgument(0, Integer.class)
          .equals(SysGuiProxyConstants.SINGLE_TREE_SELECTION.intValue()));
      return null;
    }).when(control).setSelectionMode(anyInt());
    doAnswer(invocation -> expanded.add(invocation.getArgument(0))).when(control)
        .expandNode(anyInt());
    doAnswer(invocation -> expanded.remove(invocation.getArgument(0, Integer.class))).when(control)
        .collapseNode(anyInt());
    when(control.isNodeExpanded(anyInt()))
        .thenAnswer(invocation -> expanded.contains(invocation.getArgument(0)));
    doAnswer(invocation -> {
      if (single.get()) {
        selected.clear();
      }

      return selected.contains(invocation.getArgument(0, Integer.class))
          || selected.add(invocation.getArgument(0));
    }).when(control).selectNode(anyInt());
    doAnswer(invocation -> selected.remove(invocation.getArgument(0, Integer.class))).when(control)
        .deselectNode(anyInt());
    when(control.getSelectedNode())
        .thenAnswer(invocation -> selected.isEmpty() ? -1 : selected.get(0));
    when(control.getSelectedNodes()).thenAnswer(invocation -> new BBjVector(
        selected.stream().map(BasisNumber::createBasisNumber).toList()));

    return control;
  }

  private static BBjListBox createListBox() throws BBjException {
    BBjListBox control = mock(BBjListBox.class);
    List<Integer> selected = new ArrayList<>();
    AtomicBoolean multiple = new AtomicBoolean();
    when(control.isVisible()).thenReturn(true);
    when(control.isEnabled()).thenReturn(true);
    doAnswer(invocation -> {
      multiple.set(invocation.getArgument(0));
      return null;
    }).when(control).setMultipleSelection(anyBoolean());
    doAnswer(invocation -> {
      if (!multiple.get()) {
        selected.clear();
      }

      return selected.add(invocation.getArgument(0));
    }).when(control).selectIndex(anyInt());
    doAnswer(invocation -> {
      selected.clear();
      for (Object index : (BBjVector) invocation.getArgument(0)) {
        selected.add((Integer) index);
      }

      return null;
    }).when(control).setSelectedIndices(any(BBjVector.class));
    doAnswer(invocation -> selected.remove(invocation.getArgument(0, Integer.class))).when(control)
        .deselectIndex(anyInt());
    doAnswer(invocation -> {
      selected.clear();
      return null;
    }).when(control).deselectAll();
    when(control.getSelectedIndex())
        .thenAnswer(invocation -> selected.isEmpty() ? -1 : selected.get(0));
    when(control.getSelectedIndices())
        .thenAnswer(invocation -> new BBjVector(new ArrayList<>(selected)));

    return control;
  }

  private static BBjNavigator createNavigator() throws BBjException {
    BBjNavigator control = mock(BBjNavigator.class);
    when(control.isVisible()).thenReturn(true);
    when(control.isEnabled()).thenReturn(true);

    return control;
  }

  private static BBjWebComponent createWebComponent() throws BBjException {
    BBjWebComponent control = mock(BBjWebComponent.class);
    Map<String, Object> properties = new HashMap<>();
    AtomicReference<String> text = new AtomicReference<>();
    AtomicBoolean visible = new AtomicBoolean(true);
    AtomicBoolean enabled = new AtomicBoolean(true);
    doAnswer(invocation -> {
      visible.set(invocation.getArgument(0));
      return null;
    }).when(control).setVisible(anyBoolean());
    when(control.isVisible()).thenAnswer(invocation -> visible.get());
    doAnswer(invocation -> {
      enabled.set(invocation.getArgument(0));
      return null;
    }).when(control).setEnabled(anyBoolean());
    when(control.isEnabled()).thenAnswer(invocation -> enabled.get());
    doAnswer(invocation -> {
      text.set(invocation.getArgument(0));
      return null;
    }).when(control).setText(anyString());
    when(control.getText()).thenAnswer(invocation -> text.get());
    BBjWebEventOptions eventOptions = mock(BBjWebEventOptions.class);
    when(control.newEventOptions()).thenReturn(eventOptions);
    doAnswer(invocation -> properties.put(invocation.getArgument(0), invocation.getArgument(1)))
        .when(control).setProperty(anyString(), any());
    doAnswer(invocation -> properties.put(invocation.getArgument(0), invocation.getArgument(1)))
        .when(control).setProperty(anyString(), any(), any(Type.class));
    when(control.getProperty(anyString(), any(Type.class))).thenAnswer(invocation -> {
      return properties.get(invocation.getArgument(0));
    });

    return control;
  }
}
