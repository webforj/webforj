package com.webforj.devtools.livereload.fixture;

import com.webforj.component.Component;
import com.webforj.component.window.Window;

/**
 * A route that references a card no other route references.
 */
public class OtherView extends Component {
  private final OtherCard card = new OtherCard();

  @Override
  protected void onCreate(Window window) {}

  @Override
  protected void onDestroy() {}
}
