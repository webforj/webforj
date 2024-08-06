package com.webforj.router.views;

import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.router.annotation.Route;

@NodeName("view-low-priority")
@Route(value = "low-priority", priority = 1)
class LowPriorityViewComponent extends ElementCompositeContainer {

}
