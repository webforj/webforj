window.Dwcj = window.Dwcj || {};
Dwcj.WcConnector = (() => {
  const getHV = id => document.querySelector(`[dwcj-hv="${id}"]`);
  const getEl = id => document.querySelector(`[dwcj-wc="${id}"]`);
  const wrap = body => {
    if (!body.includes('return')) {
      return `return ${body};`;
    }

    return body;
  }

  const stringifyEvent = (e) => {
    const clone = {};
    for (let k in e) {
      clone[k] = e[k];
    }

    return JSON.stringify(clone, (k, v) => {
      if (v instanceof Node) return v.nodeName;
      if (v instanceof Window) return 'Window';
      return v;
    }, ' ');
  }

  const executeExpression = (id, expression) => {
    const hv = getHV(id);
    const component = getEl(id);
    return new Function( // NOSONAR
      'component', 'hv', wrap(atob(expression))
    )(component, hv);
  }

  function setWhenLoaded (webcomponent, prop, val, count) {
      // console.log("setWhenLoaded"+webcomponent+"."+prop+"="+val+"; try "+count);
      count=count+1;

      //retry 20 times
      if (count < 20 ) {
        //first check if component itself is there
        if ( typeof webcomponent === 'undefined')
        {
          setTimeout (function () {
            setWhenLoaded (webcomponent, prop, val, count);
          }, 25);
          return;
        }

        // then check if property is available
        // TODO: what to do when property is undefined by default, is there a different was to check?
        if ( typeof webcomponent[prop] === 'undefined' )
          {
            setTimeout (function () {
              setWhenLoaded (webcomponent, prop, val, count);
              }, 25);
            return;
          }

      }
      webcomponent[prop] = val ;
  }

  return {
    /**
     * Invoke a method on the component with the given id and arguments.
     *
     * The method name can be: An actual method name, "This" or "Expression".
     *
     * 1. If the method name is "This", the first argument is the property name and the second argument is the value to set.
     * 2. If the method name is "Expression", the first argument is the expression to execute.
     * 3. If the method name is an actual method name, the arguments are passed to the method.
     *
     * @param {string} id the id of the component
     * @param {string} method  the method to invoke
     * @param  {...any} args  the arguments to pass to the method
     *
     * @returns the result of the method invocation
     */
    invoke: (id, method, ...args) => {
      const hv = getHV(id);
      const component = getEl(id);
      const m = method.toLowerCase();

      // set or get property
      if (m === 'this') {
        const len = args.length;
        if (len === 1) {
          // get property
          return component[args[0]];
        } else if (len === 2) {
          // set property
          // component[args[0]] = args[1];
          setWhenLoaded(component, args[0], args[1],0);
        }
      }

      // execute an expression
      else if (m === 'exp') {
        executeExpression(id, args[0]);
      }

      // invoke a method on the component
      else {
        const body = `component.${method}(${args.map((_, i) => `__args__[${i}]`).join(',')})`;
        return new Function( // NOSONAR
          'component', 'hv', '__args__', body
        )(component, hv, args);
      }
    },

    /**
     * Add an event listener to the component with the given id.
     *
     * @param {string} id the id of the component
     * @param {string} eventName the name of the event to listen to
     * @param {object} options the options to configure the event listener
     * @param {string} options.preventDefault the expression to determine if the event should be prevented
     * @param {string} options.stopPropagation the expression to determine if the event should be stopped
     * @param {string} options.stopImmediatePropagation the expression to determine if the event should be stopped immediately
     * @param {string} options.isAccepted the expression to determine if the event should be accepted
     * @param {string} options.detail the expression to determine the detail of the event
     */
    addEventListener: (id, eventName, options = null) => {
      const hv = getHV(id);
      const component = getEl(id);

      if (component[`__dwcj__${eventName}__h`]) {
        console.warn(`Duplicated listener registration for event "${eventName}"`, id);
        return; // stop
      }

      if (!hv || !hv.basisDispatchCustomEvent) {
        console.warn(`"basisDispatchCustomEvent" is not defined`, id);
        return; // stop
      }

      const defaultOptions = Object.assign({
        filter: null,
        detail: null,
        preventDefault: null,
        stopPropagation: null,
        stopImmediatePropagation: null,
      }, options);

      const handler = event => {
        if (defaultOptions.preventDefault && executeExpression(id, defaultOptions.preventDefault)) {
          event.preventDefault();
        }

        if (defaultOptions.stopPropagation && executeExpression(id, defaultOptions.stopPropagation)) {
          event.stopPropagation();
        }

        if (defaultOptions.stopImmediatePropagation && executeExpression(id, defaultOptions.stopImmediatePropagation)) {
          event.stopImmediatePropagation();
        }

        if (defaultOptions.filter && !executeExpression(id, defaultOptions.filter)) {
          return; // stop
        }

        if (defaultOptions.detail) {
          // allow to modify the event detail
          Object.defineProperty(event, 'detail', {
            writable: true,
            configurable: true,
            enumerable: true,
            value: event.detail
          });

          event.detail = executeExpression(id, defaultOptions.detail);
        }

        hv.basisDispatchCustomEvent(hv, {
          type: eventName,
          detail: JSON.parse(stringifyEvent(event)),
        });
      }

      component[`__dwcj__${eventName}__h`] = handler;
      component.addEventListener(eventName, handler);
    },

    /**
     * Remove an event listener from the component with the given id.
     *
     * @param {string} id the id of the component
     * @param {string} eventName the name of the event to remove the listener from
     */
    removeEventListener: (id, eventName) => {
      const component = getEl(id);
      const handler = component[`__dwcj__${eventName}__h`];
      if (!handler) {
        console.warn(`[Dwcj.WcConnector] no event listener found for event "${eventName}" on component with id "${id}"`);
        return;
      }

      component.removeEventListener(eventName, handler);
      delete component[`__dwcj__${eventName}__h`];
    }
  }
})();
