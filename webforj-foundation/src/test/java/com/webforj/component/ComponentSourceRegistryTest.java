package com.webforj.component;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;

import com.basis.webforj.BasisRegistryProbe;
import com.webforj.SourceRegistryProbe;
import com.webforj.component.ComponentSourceRegistry.SourceFrame;
import com.webforj.component.ComponentSourceRegistry.SourcePoint;
import com.webforj.environment.ObjectTable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import sun.webforj.SunRegistryProbe;

class ComponentSourceRegistryTest {

  private final Map<String, Object> table = new HashMap<>();
  private MockedStatic<ObjectTable> objectTable;

  @BeforeEach
  void setUp() {
    objectTable = mockStatic(ObjectTable.class);
    objectTable.when(() -> ObjectTable.contains(anyString()))
        .thenAnswer(call -> table.containsKey(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.get(anyString()))
        .thenAnswer(call -> table.get(call.getArgument(0)));
    objectTable.when(() -> ObjectTable.put(anyString(), any()))
        .thenAnswer(call -> table.put(call.getArgument(0), call.getArgument(1)));
  }

  @AfterEach
  void tearDown() {
    objectTable.close();
  }

  @Test
  void shouldReturnNullForUnregisteredComponent() {
    assertNull(ComponentSourceRegistry.getSourcePoint(new Object()));
  }

  @Test
  void shouldReturnEmptyChainForUnregisteredComponent() {
    Object component = new Object();

    assertTrue(ComponentSourceRegistry.getSourceChain(component).isEmpty());
    assertTrue(ComponentSourceRegistry.getSourceFrames(component).isEmpty());
    assertEquals(-1, ComponentSourceRegistry.getCreationTime(component));
  }

  @Test
  void shouldRegisterAndFindSourcePoint() {
    Object component = SourceRegistryProbe.create();

    SourcePoint result = ComponentSourceRegistry.getSourcePoint(component);

    assertNotNull(result);
    assertEquals("com.webforj.SourceRegistryProbe", result.className());
    assertEquals("SourceRegistryProbe.java", result.fileName());
    assertTrue(result.lineNumber() > 0);
    assertEquals(new SourcePoint(result.className(), result.fileName(), result.lineNumber()),
        result);
  }

  @Test
  void shouldCreateStorageIfNotExists() {
    ComponentSourceRegistry.register(new Object());

    objectTable.verify(() -> ObjectTable.put(eq(ComponentSourceRegistry.class.getName()), any()));
  }

  @Test
  void shouldReturnChainInStackOrderWithTheRunningInstruction() {
    Object component = SourceRegistryProbe.createThrough();

    List<SourceFrame> frames = ComponentSourceRegistry.getSourceFrames(component);

    assertEquals(List.of("create", "createThrough"),
        frames.stream().limit(2).map(SourceFrame::getMethodName).toList());
    assertEquals(List.of("()Ljava/lang/Object;", "()Ljava/lang/Object;"),
        frames.stream().limit(2).map(SourceFrame::getDescriptor).toList());
    assertTrue(frames.stream().limit(2).allMatch(frame -> frame.getBytecodeIndex() >= 0));

    List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);

    assertEquals(chain, frames.stream().map(SourceFrame::getSourcePoint).toList());
    assertEquals(ComponentSourceRegistry.getSourcePoint(component), chain.get(0));
    assertTrue(ComponentSourceRegistry.getCreationTime(component) > 0);
  }

  @Test
  void shouldExcludeFilteredPackagesFromChain() {
    List<Object> components = List.of(SourceRegistryProbe.create(), BasisRegistryProbe.create(),
        SunRegistryProbe.create());

    List<String> classes = components.stream().map(ComponentSourceRegistry::getSourceChain)
        .flatMap(List::stream).map(SourcePoint::className).toList();

    assertTrue(classes.contains("com.webforj.SourceRegistryProbe"), classes.toString());
    assertTrue(classes.stream()
        .noneMatch(name -> name.startsWith("com.webforj.component.") || name.startsWith("java.")
            || name.startsWith("jdk.") || name.startsWith("sun.") || name.startsWith("com.basis.")),
        classes.toString());
  }

  @Test
  void shouldCapChainAtTenEntries() {
    Object component = SourceRegistryProbe.createThrough();

    assertEquals(10, ComponentSourceRegistry.getSourceChain(component).size());
  }

  @Test
  void shouldReturnChainTheCallerMayChange() {
    Object component = SourceRegistryProbe.create();
    List<SourcePoint> chain = ComponentSourceRegistry.getSourceChain(component);
    int size = chain.size();
    chain.clear();

    assertEquals(size, ComponentSourceRegistry.getSourceChain(component).size());
  }

  @Test
  void shouldKeepEqualComponentsApart() {
    String first = new String("same");
    String second = new String("same");

    ComponentSourceRegistry.register(first);

    assertNotNull(ComponentSourceRegistry.getSourcePoint(first));
    assertNull(ComponentSourceRegistry.getSourcePoint(second));
  }
}
