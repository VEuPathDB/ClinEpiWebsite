import React from 'react';

import { IconAlt as Icon } from 'wdk-client/Components';

class NavItem extends React.Component {
  constructor (props) {
    super(props);
    this.state = { isHovered: false };
    this.onMouseEnter = this.onMouseEnter.bind(this);
    this.onMouseLeave = this.onMouseLeave.bind(this);
  }

  onMouseEnter (event) {
    this.setState({ isHovered: true });
  }

  onMouseLeave (event) {
    this.setState({ isHovered: false });
  }

  render () {
    const { onMouseEnter, onMouseLeave } = this;
    const { isHovered } = this.state;
    const { item, config } = this.props;
    const { id, text, url, appUrl } = item;
    const { webAppUrl, projectId } = config;

    const children = (typeof item.children === 'function')
      ? item.children({ webAppUrl, projectId })
      : item.children;

    const destination = appUrl && appUrl.length
      ? webAppUrl + appUrl
      : url && url.length
        ? url
        : null;

    const className = 'NavItem' + (children && children.length ? ' NavItem--HasSubmenu' : '');

    return (
      <box className={className} key={id} onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave}>
      	{destination
          ? <a className="NavItem-Link" href={destination}>{text}</a>
          : <span className="NavItem-Text">{text}</span>
        }
        {children && children.length
          ? <Icon fa="caret-down" />
          : null
        }
        {children && children.length
          ? (
            <stack className={'NavItem-Submenu' + (isHovered ? '' : ' NavItem-Submenu--hidden')}>
              {children.map((child, idx) => (
                <NavItem
                  item={child}
                  config={config}
                  key={idx}
                />
              ))}
            </stack>
          )
          : null
        }
      </box>
    );
  }
};

export default NavItem;