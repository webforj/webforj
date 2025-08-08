package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.BBjAPI;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.environment.ObjectTable;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class BootstrapContextTest {

  private BBjAPI mockApi;
  private WebforjBBjBridge mockBridge;
  private BootstrapContext context;
  private static final String TEST_ID = "test-bootstrap-id";

  @BeforeEach
  void setUp() {
    mockApi = mock(BBjAPI.class);
    mockBridge = mock(WebforjBBjBridge.class);
    context = new BootstrapContext(mockApi, mockBridge, 1, "com.example.TestApp", TEST_ID);
  }

  @Test
  void shouldReturnConstructorParameters() {
    assertSame(mockApi, context.getApi());
    assertSame(mockBridge, context.getBridge());
    assertEquals(1, context.getDebug());
    assertEquals("com.example.TestApp", context.getRequestedClassName());
    assertEquals(TEST_ID, context.getId());
  }

  @Test
  void shouldHandleNullRequestedClassName() {
    BootstrapContext contextWithNull = new BootstrapContext(mockApi, mockBridge, 0, null, TEST_ID);

    assertNull(contextWithNull.getRequestedClassName());
    assertEquals(0, contextWithNull.getDebug());
  }

  @Test
  void shouldStartWithNullConfiguration() {
    assertNull(context.getConfiguration());
  }

  @Test
  void shouldSetAndGetConfiguration() {
    Config config = ConfigFactory.parseString("test.key = \"value\"");
    context.setConfiguration(config);

    assertSame(config, context.getConfiguration());
  }

  @Test
  void shouldReplaceConfiguration() {
    Config originalConfig = ConfigFactory.parseString("original.key = \"original\"");
    Config newConfig = ConfigFactory.parseString("new.key = \"new\"");

    context.setConfiguration(originalConfig);

    try (MockedStatic<ObjectTable> objectTableMock = Mockito.mockStatic(ObjectTable.class)) {
      context.replaceConfiguration(newConfig);

      assertSame(newConfig, context.getConfiguration());
      objectTableMock.verify(() -> ObjectTable.put("webforj.configuration", newConfig));
    }
  }

  @Test
  void shouldMergeConfiguration() {
    Config baseConfig = ConfigFactory.parseString("base.key = \"base\"\nshared.key = \"base\"");
    Config additionalConfig =
        ConfigFactory.parseString("additional.key = \"additional\"\nshared.key = \"override\"");

    context.setConfiguration(baseConfig);

    try (MockedStatic<ObjectTable> objectTableMock = Mockito.mockStatic(ObjectTable.class)) {
      Config mergedConfig = context.mergeConfiguration(additionalConfig);

      // Additional config should override base
      assertEquals("override", mergedConfig.getString("shared.key"));
      // Both configs should be present
      assertEquals("base", mergedConfig.getString("base.key"));
      assertEquals("additional", mergedConfig.getString("additional.key"));

      assertSame(mergedConfig, context.getConfiguration());
      objectTableMock.verify(() -> ObjectTable.put("webforj.configuration", mergedConfig));
    }
  }

  @Test
  void shouldSetAndGetEntry() {
    assertNull(context.getEntry());

    context.setEntry("com.example.MyApp");
    assertEquals("com.example.MyApp", context.getEntry());
  }

  @Test
  void shouldManageAttributes() {
    assertFalse(context.hasAttribute("test-key"));
    assertNull(context.getAttribute("test-key"));

    context.setAttribute("test-key", "test-value");

    assertTrue(context.hasAttribute("test-key"));
    assertEquals("test-value", context.getAttribute("test-key"));

    context.setAttribute("test-key", "new-value");
    assertEquals("new-value", context.getAttribute("test-key"));
  }

  @Test
  void shouldGetTypedAttributes() {
    context.setAttribute("string-key", "string-value");
    context.setAttribute("integer-key", 42);
    context.setAttribute("boolean-key", true);

    // Correct types
    assertEquals("string-value", context.getAttribute("string-key", String.class));
    assertEquals(42, context.getAttribute("integer-key", Integer.class));
    assertEquals(true, context.getAttribute("boolean-key", Boolean.class));

    // Wrong types should return null
    assertNull(context.getAttribute("string-key", Integer.class));
    assertNull(context.getAttribute("integer-key", String.class));

    // Non-existent key
    assertNull(context.getAttribute("non-existent", String.class));
  }

  @Test
  void shouldHandleNullAttributes() {
    context.setAttribute("test-key", "test-value");
    assertTrue(context.hasAttribute("test-key"));

    context.setAttribute("test-key", null);
    assertFalse(context.hasAttribute("test-key"));
    assertNull(context.getAttribute("test-key"));
    assertNull(context.getAttribute("test-key", String.class));
  }

  @Test
  void shouldSupportComplexAttributeTypes() {
    TestComplexObject complexObject = new TestComplexObject("test", 100);
    context.setAttribute("complex", complexObject);

    TestComplexObject retrieved = context.getAttribute("complex", TestComplexObject.class);
    assertNotNull(retrieved);
    assertEquals("test", retrieved.getName());
    assertEquals(100, retrieved.getValue());

    // Wrong type
    assertNull(context.getAttribute("complex", String.class));
  }

  static class TestComplexObject {
    private final String name;
    private final int value;

    TestComplexObject(String name, int value) {
      this.name = name;
      this.value = value;
    }

    public String getName() {
      return name;
    }

    public int getValue() {
      return value;
    }
  }
}
