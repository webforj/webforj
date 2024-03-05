package com.webforj.component.slider.sink;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.Environment;

import com.webforj.bridge.ComponentAccessor;
import com.webforj.component.slider.Slider;
import com.webforj.component.slider.event.SliderScrollEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;



public class SliderScrollEventSink {

  private final ArrayList<Consumer<SliderScrollEvent>> targets = new ArrayList<>();

  private final Slider slider;

  private BBjControl bbjControl;

  @SuppressWarnings({"static-access"})
  public SliderScrollEventSink(Slider slide) {
    this.slider = slide;

    try {
      bbjControl = ComponentAccessor.getDefault().getBBjControl(slide);
      bbjControl.setCallback(Environment.getCurrent().getBBjAPI().ON_CONTROL_SCROLL,
          Environment.getCurrent().getDwcjHelper().getEventProxy(this, "onScrollEvent"), "onEvent");

    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  @SuppressWarnings("java:S1172")
  public void onScrollEvent(BBjControlScrollEvent ev) { // NOSONAR

    SliderScrollEvent dwcEv = new SliderScrollEvent(this.slider);
    Iterator<Consumer<SliderScrollEvent>> it = targets.iterator();
    while (it.hasNext()) {
      it.next().accept(dwcEv);
    }
  }

  public void addCallback(Consumer<SliderScrollEvent> callback) {
    targets.add(callback);
  }

}
