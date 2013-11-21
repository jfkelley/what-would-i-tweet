create table access_tokens(
	user_id bigint primary key,
	token varchar,
	secret varchar
);