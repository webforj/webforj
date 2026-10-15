package com.webforj.devtools.livereload.fixture;

import com.webforj.component.Composite;
import com.webforj.component.html.elements.Div;
import com.webforj.router.annotation.Route;

/**
 * A route rendered in the reports route.
 */
@Route(value = "chart", outlet = ReportsView.class)
public class ReportsChartView extends Composite<Div> {
}
