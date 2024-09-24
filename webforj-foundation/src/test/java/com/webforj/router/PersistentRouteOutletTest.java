package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import com.webforj.component.window.Window;
import com.webforj.concern.HasComponents;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class PersistentRouteOutletTest {

  Container container;

  @BeforeEach
  void setUp() {
    container = new Container();
  }

  @Test
  void shouldShowRouteContent() {
    PersistentRouteOutlet outlet = new PersistentRouteOutlet(container);
    Component component = mock(Component.class);
    outlet.showRouteContent(component);
    assertTrue(container.hasComponent(component));
  }

  @Test
  void shouldShowRouteContentInComposite() {
    CompositeContainer composite = new CompositeContainer();
    PersistentRouteOutlet outlet = new PersistentRouteOutlet(composite);
    Component component = mock(Component.class);
    outlet.showRouteContent(component);
    assertTrue(composite.hasComponent(component));
  }

  static class Container extends Component implements HasComponents {
    List<Component> components = new ArrayList<>();

    @Override
    protected void onCreate(Window window) {
      // no-op
    }

    @Override
    protected void onDestroy() {
      // no-op
    }

    @Override
    public void add(Component... components) {
      for (Component component : components) {
        this.components.add(component);
      }
    }

    @Override
    public boolean hasComponent(Component component) {
      return components.contains(component);
    }
  }

  static class CompositeContainer extends Composite<Container> {
    public boolean hasComponent(Component component) {
      return getBoundComponent().hasComponent(component);
    }
  }
}
