package com.webforj.router.views;

import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.router.annotation.Route;

@NodeName("view-high-priority")
@Route(value = "high-priority", priority = 10)
class HighPriorityViewComponent extends ElementCompositeContainer {

}
