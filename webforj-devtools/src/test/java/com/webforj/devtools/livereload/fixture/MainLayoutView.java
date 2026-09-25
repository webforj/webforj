package com.webforj.devtools.livereload.fixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.router.RouteOutlet;

/**
 * A route outlet that references a badge and a navigation target.
 */
public class MainLayoutView extends Component implements RouteOutlet {
  private final LayoutBadge badge = new LayoutBadge();
  private final Class<?> navigationTarget = DashboardView.class;

  @Override
  protected void onCreate(Window window) {}

  @Override
  protected void onDestroy() {}

  @Override
  public void showRouteContent(Component component) {}

  @Override
  public void removeRouteContent(Component component) {}
}
