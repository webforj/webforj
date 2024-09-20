package com.webforj.conceiver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.window.Window;
import com.webforj.conceiver.exception.ConceiverException;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjException;
import java.util.Collections;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class DefaultConceiverTest {

  DefaultConceiver conceiver;
  MyComponent mockComponent;
  Map<String, Object> mockData = Collections.emptyMap();

  @BeforeEach
  void setUp() {
    conceiver = new DefaultConceiver();
    mockComponent = new MyComponent();
  }

  @Test
  void shouldCreateInstanceWithGet() {
    MyClass result = conceiver.get(MyClass.class);
    assertNotNull(result);
    assertTrue(result instanceof MyClass);
  }

  @Test
  void shouldThrowConceiverExceptionWhenInstantiationFails() {
    assertThrows(ConceiverException.class,
        () -> conceiver.get(ClassWithNoDefaultConstructor.class));
  }

  @Test
  void shouldCreateInstanceWithGetApplication() throws WebforjAppInitializeException {
    App app = conceiver.getApplication(TestApp.class);
    assertNotNull(app);
    assertTrue(app instanceof TestApp);
  }

  @Test
  void shouldCreateInstanceWithGetComponent() {
    MyComponent component = conceiver.getComponent(MyComponent.class);
    assertNotNull(component);
    assertTrue(component instanceof MyComponent);
  }

  @Nested
  class CreateComponentEvent {

    @Test
    void shouldCreateComponentEventWithTwoArgsConstructor() {
      ComponentEvent<?> event =
          conceiver.getComponentEvent(mockComponent, EventWithTwoArgs.class, mockData);

      assertNotNull(event);
      assertTrue(event instanceof EventWithTwoArgs);
      assertEquals(mockComponent, event.getComponent());
      assertEquals(mockData, event.getData());
    }

    @Test
    void shouldCreateComponentEventWithThreeArgsConstructorForInnerClass() {
      ComponentEvent<?> event =
          conceiver.getComponentEvent(mockComponent, MyComponent.InnerEvent.class, mockData);

      assertNotNull(event);
      assertTrue(event instanceof MyComponent.InnerEvent);
      assertEquals(mockComponent, event.getComponent());
      assertEquals(mockData, event.getData());
    }

    @Test
    void shouldThrowExceptionWhenConstructorInvocationFails() {
      assertThrows(ConceiverException.class,
          () -> conceiver.getComponentEvent(mockComponent, FailingEvent.class, mockData));
    }
  }

  static class MyClass {
    public MyClass() {
      // no-op
    }
  }

  class TestApp extends App {
    @Override
    public void run() throws WebforjException {
      // no-op
    }
  }


  class MyComponent extends Component {
    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }

    public class InnerEvent extends ComponentEvent<Component> {
      public InnerEvent(Component source, Map<String, Object> data) {
        super(source, data);
      }
    }
  }

  class ClassWithNoDefaultConstructor {
    private ClassWithNoDefaultConstructor() {
      // no-op
    }
  }

  // Event class where constructor throws an exception
  public static class FailingEvent extends ComponentEvent<Component> {
    public FailingEvent(Component source, Map<String, Object> data) {
      super(source, data);
      throw new RuntimeException("Constructor failure");
    }
  }
}
