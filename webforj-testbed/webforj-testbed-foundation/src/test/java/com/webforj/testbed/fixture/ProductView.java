package com.webforj.testbed.fixture;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;
import com.webforj.router.event.DidEnterEvent;
import com.webforj.router.history.ParametersBag;
import com.webforj.router.observer.DidEnterObserver;

/** Reads the product id from its route for navigation tests. */
@Route("product/:id")
public class ProductView extends Composite<Div> implements DidEnterObserver {
  private String productId;

  @Override
  public void onDidEnter(DidEnterEvent event, ParametersBag parameters) {
    productId = parameters.get("id").orElse(null);
  }

  public String getProductId() {
    return productId;
  }
}
