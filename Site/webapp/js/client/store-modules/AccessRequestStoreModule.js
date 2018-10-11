import { merge } from 'rxjs';
import { filter, mergeAll, mergeMap, withLatestFrom } from 'rxjs/operators';

import { 
  FINISH_SUBMISSION,
  SUBMIT_FORM,
  UPDATE_FIELD, 
  UPDATE_STUDY, 
  UPDATE_LOADING_ERROR, 
  UPDATE_SUBMISSION_ERROR,
  UPDATE_USER_ID,
  finishSubmission,
  updateField,
  updateLoadingError,
  updateStudy,
  updateUserId
} from '../action-creators/AccessRequestActionCreators';
import { datasetId, formValues, userId } from '../selectors/AccessRequestSelectors';
import { parse } from 'querystring';

const initialState = {
  userId: null,
  study: null,
  loadingError: null,
  submitting: false,
  successfullySubmitted: false,
  alreadyRequested: false,
  submissionError: null,
  formValues: {
    'request_date': '',
    'requester_name': '',
    'requester_email': '',
    'organization': '',
    'purpose': '',
    'research_question': '',
    'analysis_plan': '',
    'dissemination_plan': ''
  }
};

const ALREADY_REQUESTED_STATUS = 409;

export function reduce(state = initialState, { type, payload }) {
  switch(type) {
    case UPDATE_USER_ID:
      return {
        ...state,
        userId: payload.userId
      };

    case UPDATE_STUDY:
      return {
        ...state,
        study: payload.study
      };

    case UPDATE_LOADING_ERROR:
      return {
        ...state,
        loadingError: payload.loadingError
      }

    case UPDATE_SUBMISSION_ERROR:
      return {
        ...state,
        submissionError: payload.submissionError
      };

    case UPDATE_FIELD:
      return {
        ...state,
        formValues: {
          ...state.formValues,
          [payload.key]: payload.value || ''
        }
      }

    case SUBMIT_FORM:
      return {
        ...state,
        submitting: true
      };

    case FINISH_SUBMISSION:
      return {
        ...state,
        submitting: false,
        successfullySubmitted: payload.successfullySubmitted,
        alreadyRequested: payload.alreadyRequested,
        submissionError: payload.submissionError
      }

    default: 
      return state;
  }
}

export function observe(action$, state$, dependencies) {
  return merge(
    observeStaticDataLoaded(action$, state$, dependencies),
    observeSubmitForm(action$, state$, dependencies)
  );
}

function observeStaticDataLoaded(action$, state$, dependencies) {
  return action$.pipe(
    filter(({ type }) => type === 'static/all-data-loaded'),
    mergeMap(async ({ payload }) => {
      const onRequestAccessRoute = window.location.pathname.includes('/request-access/');

      if (!onRequestAccessRoute) {
        return [];
      }

      const datasetId = window.location.pathname.replace(/.*\/request-access\//, '');

      if (
        payload.user.isGuest || payload.user.properties.approvedStudies.includes(datasetId)
      ) {
        const { redirectUrl = '/' } = parse(window.location.search.slice(1));

        window.location.assign(decodeURIComponent(redirectUrl));
      } else {
        try {
          const study = await fetchStudy(
            datasetId,
            dependencies.wdkService
          );

          const {
            id,
            email = '',
            properties: { firstName, middleName, lastName, organization } = 
              { firstName: '', middleName: '', lastName: '', organization: '' }
          } = payload.user;

          return [
            updateUserId(id),
            updateField('request_date', new Date().toLocaleDateString('en-US')),
            updateField('requester_name', `${firstName} ${middleName} ${lastName}`.replace(/\s+/g, ' ')),
            updateField('requester_email', email),
            updateField('organization', organization),
            updateStudy(study)
          ];
        }
        catch (loadingError) {
          return [
            updateLoadingError(loadingError)
          ];
        }
      }
    }),
    mergeAll()
  );
}

function observeSubmitForm(action$, state$, dependencies) {
  return action$.pipe(
    filter(({ type }) => type === SUBMIT_FORM),
    withLatestFrom(state$),
    mergeMap(async ([, state]) => {
      const response = await jsonPutRequest(
        dependencies.wdkService.serviceUrl,
        `/users/${userId(state)}/access-request/${datasetId(state)}`,
        formValues(state)
      );

      if (response.ok) {
        return finishSubmission(true, false, null);
      } else if (response.status === ALREADY_REQUESTED_STATUS) {
        return finishSubmission(false, true, null);
      } else {
        return finishSubmission(false, false, await response.text())
      }
    })
  );
}

const fetchStudy = (datasetId, wdkService) => wdkService.getRecord(
  'DatasetRecordClasses.DatasetRecordClass',
  [
    {
      name: 'dataset_id',
      value: datasetId
    }
  ],
  {
    attributes: [
      'dataset_id',
      'display_name',
      'request_access_fields'
    ],
    tables: []
  }
);

const jsonPutRequest = (serviceUrl, endpoint, body) => fetch(
  `${serviceUrl}${endpoint}`,
  {
    method: 'PUT',
    headers: {
      'credentials': 'include',
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(body)
  }
);