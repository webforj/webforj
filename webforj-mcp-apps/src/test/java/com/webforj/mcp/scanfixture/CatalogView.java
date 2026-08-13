package com.webforj.mcp.scanfixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.mcp.annotation.McpApp;
import com.webforj.router.annotation.Route;

@Route("/catalog")
@McpApp(description = "Shows the product catalog")
public class CatalogView extends Component {

  @Override
  protected void onCreate(Window window) {}

  @Override
  protected void onDestroy() {}
}
