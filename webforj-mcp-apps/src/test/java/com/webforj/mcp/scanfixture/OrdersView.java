package com.webforj.mcp.scanfixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.router.annotation.Route;

@Route("/orders")
public class OrdersView extends Component {

  @Override
  protected void onCreate(Window window) {
    // Fixture, never rendered
  }

  @Override
  protected void onDestroy() {
    // Fixture, never rendered
  }
}
