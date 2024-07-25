package com.webforj.router;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import org.junit.jupiter.api.Test;

class ViewNameGeneratorTest {

  @Test
  void shouldReturnEmptyStringForMainView() {
    assertEquals("", ViewNameGenerator.generate(MainView.class));
  }

  @Test
  void shouldReturnEmptyStringForMain() {
    assertEquals("", ViewNameGenerator.generate(Main.class));
  }

  @Test
  void shouldRemoveViewSuffixAndLowerCase() {
    assertEquals("dashboard", ViewNameGenerator.generate(DashboardView.class));
  }

  @Test
  void shouldLowerCaseClassName() {
    assertEquals("settings", ViewNameGenerator.generate(Settings.class));
  }

  class MainView extends Component {
    @Override
    protected void onCreate(Window window) {
      // do nothing
    }

    @Override
    protected void onDestroy() {}
    // do nothing
  }

  class Main extends Component {
    @Override
    protected void onCreate(Window window) {
      // do nothing
    }

    @Override
    protected void onDestroy() {
      // do nothing
    }
  }

  class DashboardView extends Component {
    @Override
    protected void onCreate(Window window) {
      // do nothing
    }

    @Override
    protected void onDestroy() {
      // do nothing
    }
  }


  class Settings extends Component {
    @Override
    protected void onCreate(Window window) {
      // do nothing
    }

    @Override
    protected void onDestroy() {
      // do nothing
    }

  }
}
