package com.webforj.spring.testbed.fixture;

import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;

/** Displays an injected greeting for Spring testbed tests. */
@Route("greeting")
public class GreetingView extends Composite<Div> {
  private final Button button;

  public GreetingView(GreetingService service) {
    button = new Button(service.greet());
    getBoundComponent().add(button);
  }

  public Button getButton() {
    return button;
  }
}
