package com.webforj.testbed.catalog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Component;
import java.lang.reflect.Method;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.MethodSource;

class FoundationCoverageGuardTest {

  @Test
  void shouldVerifyEveryFoundationComponent() throws NoSuchMethodException {
    List<Class<? extends Component>> catalog = FoundationComponents.discover();
    assertTrue(catalog.size() >= 30,
        "Expected the guard to discover the foundation components but found " + catalog.size());

    Method test = FoundationComponentMethodsTest.class
        .getDeclaredMethod("shouldCarryEveryPublicMethod", Class.class);
    MethodSource source = test.getAnnotation(MethodSource.class);
    assertEquals(List.of("com.webforj.testbed.catalog.FoundationComponents#discover"),
        List.of(source.value()),
        "The methods test must take every component from FoundationComponents.discover()");
  }
}
