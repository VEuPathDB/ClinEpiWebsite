-- Schema creation (if needed)
CREATE SCHEMA IF NOT EXISTS studyaccess;
GRANT USAGE ON SCHEMA studyaccess TO COMM_WDK_W;

-- approval_status
CREATE TABLE studyaccess.approval_status (
  approval_status_id INTEGER PRIMARY KEY,
  name VARCHAR(24) NOT NULL
);

-- restriction_level
CREATE TABLE studyaccess.restriction_level (
  restriction_level_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name VARCHAR(24) UNIQUE NOT NULL
);

-- staff
CREATE TABLE studyaccess.staff (
  staff_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL UNIQUE,
  is_owner BOOLEAN DEFAULT FALSE NOT NULL
);

-- providers
CREATE TABLE studyaccess.providers (
  provider_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL,
  is_manager BOOLEAN DEFAULT FALSE NOT NULL,
  dataset_id VARCHAR(15) NOT NULL,
  CONSTRAINT provider_user_ds_uq UNIQUE (user_id, dataset_id)
);

-- end_users
CREATE TABLE studyaccess.end_users (
  end_user_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  user_id BIGINT NOT NULL,
  dataset_presenter_id VARCHAR(15) NOT NULL,
  restriction_level_id INTEGER NOT NULL REFERENCES studyaccess.restriction_level(restriction_level_id),
  approval_status_id INTEGER DEFAULT 1 NOT NULL REFERENCES studyaccess.approval_status(approval_status_id),
  start_date TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
  duration BIGINT DEFAULT -1 NOT NULL,
  purpose TEXT,
  research_question TEXT,
  analysis_plan TEXT,
  dissemination_plan TEXT,
  prior_auth TEXT,
  denial_reason TEXT,
  date_denied TIMESTAMPTZ,
  allow_self_edits BOOLEAN DEFAULT FALSE NOT NULL,
  CONSTRAINT end_user_ds_user_uq UNIQUE (user_id, dataset_presenter_id)
);

-- end_user_history
CREATE TABLE studyaccess.end_user_history (
  end_user_id BIGINT NOT NULL,
  user_id BIGINT NOT NULL,
  dataset_presenter_id VARCHAR(15) NOT NULL,
  history_action VARCHAR(6) NOT NULL,
  history_timestamp TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
  history_cause_user BIGINT NOT NULL,
  restriction_level_id INTEGER NOT NULL REFERENCES studyaccess.restriction_level(restriction_level_id),
  approval_status_id INTEGER NOT NULL REFERENCES studyaccess.approval_status(approval_status_id),
  start_date TIMESTAMPTZ NOT NULL,
  duration BIGINT NOT NULL,
  purpose TEXT,
  research_question TEXT,
  analysis_plan TEXT,
  dissemination_plan TEXT,
  prior_auth TEXT,
  denial_reason TEXT,
  date_denied TIMESTAMPTZ,
  allow_self_edits BOOLEAN NOT NULL
);
