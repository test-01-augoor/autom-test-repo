import { ComponentType } from 'react';
import FullPageSpinner from 'components/FullPageSpinner';
import FullPageError from 'components/FullPageError';
import useAssistantRepositories from 'hooks/useAssistantRepositories';

export interface WithAssistantRepositoriesOptions {
  alwaysRender?: JSX.Element;
}

export interface WithAssistantRepositoriesState {
  isRefetchingStatus: boolean;
  refetchInterval?: number;
  enabled: boolean;
}

function getDisplayName<P>(WrappedComponent: ComponentType<P>): string {
  return WrappedComponent.displayName || WrappedComponent.name || 'Component';
}

export default function withAssistantRepositories<P>(
  WrappedComponent: ComponentType<P>,
  options?: WithAssistantRepositoriesOptions
): ComponentType<P> {
  function WithAssistantRepositories(props: P): JSX.Element {
    const {
      data: repositories = [],
      isLoading,
      isError: isErrorRepositories,
      error: errorRepositories,
    } = useAssistantRepositories();

    if (isLoading) {
      return (
        <>
          {options?.alwaysRender}
          <FullPageSpinner />
        </>
      );
    }

    if (isErrorRepositories && errorRepositories !== null) {
      return (
        <>
          {options?.alwaysRender}
          <FullPageError error={errorRepositories} />
        </>
      );
    }

    return (
      <>
        {options?.alwaysRender}
        <WrappedComponent repositories={repositories} {...props} />
      </>
    );
  }

  WithAssistantRepositories.displayName = `WithAssistantRepositories(${getDisplayName(
    WrappedComponent
  )})`;

  return WithAssistantRepositories;
}
