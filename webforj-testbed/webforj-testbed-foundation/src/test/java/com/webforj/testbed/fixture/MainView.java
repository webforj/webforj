package com.webforj.testbed.fixture;

import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;

/** Provides the main route for navigation tests. */
@Route("main")
public class MainView extends Composite<Div> {
  private final Button button = new Button("Hello");

  public MainView() {
    getBoundComponent().add(button);
  }

  public Button getButton() {
    return button;
  }
}
