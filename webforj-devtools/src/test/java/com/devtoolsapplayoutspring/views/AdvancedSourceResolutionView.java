package com.devtoolsapplayoutspring.views;

import com.webforj.component.Composite;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;

/** Compiled application view with local, anonymous and inherited component owners. */
public class AdvancedSourceResolutionView extends InheritedActions {

  /** Creates the local and anonymous component groups. */
  public AdvancedSourceResolutionView() {
    class LocalActions extends Composite<FlexLayout> {
      private final Button action = new Button("Local resolved action");

      LocalActions() {
        action.setTooltipText("Local tooltip");
        getBoundComponent().add(action);
      }
    }

    Composite<FlexLayout> anonymous = new Composite<>() {
      private final Button action = new Button("Anonymous resolved action");

      {
        action.setTooltipText("Anonymous tooltip");
        getBoundComponent().add(action);
      }
    };
    getBoundComponent().add(new LocalActions(), anonymous);
  }
}


class InheritedActions extends Composite<FlexLayout> {
  private final Button action = new Button("Inherited resolved action");

  InheritedActions() {
    action.setTooltipText("Inherited tooltip");
    getBoundComponent().add(action);
  }
}
