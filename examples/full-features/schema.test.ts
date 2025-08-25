import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer } from 'graphql-compose'

describe('Full Features Example', () => {
	const schemaPath = join(__dirname, './schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Custom model name via @@graphql.name', () => {
		expect(schemaComposer.has('Member')).toBe(true)
		expect(schemaComposer.has('User')).toBe(false)
		expect(schemaComposer.has('Reaction')).toBe(true)
		expect(schemaComposer.has('Like')).toBe(false)

		const member = schemaComposer.getOTC('Member')
		expect(member).toBeTruthy()
	})

	test('Custom model description via @@graphql.description', () => {
		const member = schemaComposer.getOTC('Member')
		expect(member.getDescription()).toBe('Platform member with social interactions')

		const reactionTypeETC = schemaComposer.getETC('ReactionType')
		expect(reactionTypeETC.getDescription()).toBe('Types of reactions users can leave')
	})

	test('Model is ignored via @@graphql.ignore', () => {
		expect(schemaComposer.has('SystemConfig')).toBe(false)
	})

	test('Custom field name via @graphql.name', () => {
		const member = schemaComposer.getOTC('Member')
		expect(member.hasField('signupIpAddress')).toBe(true)

		const profile = schemaComposer.getOTC('UserProfile')
		expect(profile.hasField('personalSite')).toBe(true)
		expect(profile.hasField('websiteUrl')).toBe(false)
	})

	test('Field is ignored via @graphql.ignore', () => {
		const member = schemaComposer.getOTC('Member')
		expect(member.hasField('passwordHash')).toBe(false)
		expect(member.hasField('securityAnswers')).toBe(false)
	})

	test('Custom field description via @graphql.description', () => {
		const member = schemaComposer.getOTC('Member')
		const lastActiveField = member.getField('lastActive')
		expect(lastActiveField.description).toBe("User's last activity timestamp")

		const profile = schemaComposer.getOTC('UserProfile')
		const personalSiteField = profile.getField('personalSite')
		expect(personalSiteField.description).toBe("User's personal website")
	})

	test('Connection types are generated', () => {
		expect(schemaComposer.has('MemberConnection')).toBe(true)
		expect(schemaComposer.has('MemberEdge')).toBe(true)
		expect(schemaComposer.has('PostConnection')).toBe(true)
		expect(schemaComposer.has('PostEdge')).toBe(true)

		const memberConnection = schemaComposer.getOTC('MemberConnection')
		expect(memberConnection.hasField('edges')).toBe(true)
		expect(memberConnection.hasField('pageInfo')).toBe(true)
		expect(memberConnection.hasField('totalCount')).toBe(true)

		expect(memberConnection.getFieldType('edges').toString()).toBe('[MemberEdge!]!')
		expect(memberConnection.getFieldType('pageInfo').toString()).toBe('PageInfo!')
		expect(memberConnection.getFieldType('totalCount').toString()).toBe('Int!')

		const memberEdge = schemaComposer.getOTC('MemberEdge')
		expect(memberEdge.hasField('node')).toBe(true)
		expect(memberEdge.hasField('cursor')).toBe(true)
		expect(memberEdge.getFieldType('node').toString()).toBe('Member!')
	})

	test('Sort inputs are generated for @graphql.sortable fields', () => {
		expect(schemaComposer.has('MemberSortInput')).toBe(true)
		expect(schemaComposer.has('PostSortInput')).toBe(true)
		expect(schemaComposer.has('CommentSortInput')).toBe(true)
		expect(schemaComposer.has('UserProfileSortInput')).toBe(true)

		const memberSortInput = schemaComposer.getITC('MemberSortInput')
		expect(memberSortInput.hasField('createdAt')).toBe(true)
		expect(memberSortInput.hasField('birthdate')).toBe(true)
		expect(memberSortInput.getFieldType('createdAt').toString()).toBe('SortDirection')

		const postSortInput = schemaComposer.getITC('PostSortInput')
		expect(postSortInput.hasField('createdAt')).toBe(true)
		expect(postSortInput.hasField('viewCount')).toBe(true)
		expect(postSortInput.hasField('latitude')).toBe(true)

		const userProfileSortInput = schemaComposer.getITC('UserProfileSortInput')
		expect(userProfileSortInput.hasField('karmaPoints')).toBe(true)
	})

	test('Filter inputs are generated for @graphql.filterable fields', () => {
		expect(schemaComposer.has('MemberFilterInput')).toBe(true)
		expect(schemaComposer.has('PostFilterInput')).toBe(true)
		expect(schemaComposer.has('UserProfileFilterInput')).toBe(true)
		expect(schemaComposer.has('CommentFilterInput')).toBe(true)

		const memberFilterInput = schemaComposer.getITC('MemberFilterInput')
		expect(memberFilterInput.hasField('username')).toBe(true)
		expect(memberFilterInput.hasField('fullName')).toBe(true)
		expect(memberFilterInput.getFieldType('username').toString()).toBe('StringFilterInput')

		const postFilterInput = schemaComposer.getITC('PostFilterInput')
		expect(postFilterInput.hasField('createdAt')).toBe(true)
		expect(postFilterInput.hasField('title')).toBe(true)
		expect(postFilterInput.hasField('isPublished')).toBe(true)
		expect(postFilterInput.getFieldType('isPublished').toString()).toBe('BooleanFilterInput')

		const userProfileFilterInput = schemaComposer.getITC('UserProfileFilterInput')
		expect(userProfileFilterInput.hasField('location')).toBe(true)
		expect(userProfileFilterInput.hasField('karmaPoints')).toBe(true)
		expect(userProfileFilterInput.getFieldType('karmaPoints').toString()).toBe('NumericFilterInput')
	})

	test('Complex relations are properly defined', () => {
		const member = schemaComposer.getOTC('Member')
		expect(member.hasField('posts')).toBe(true)
		expect(member.hasField('comments')).toBe(true)
		expect(member.hasField('follows')).toBe(true)
		expect(member.hasField('followers')).toBe(true)
		expect(member.getFieldType('posts').toString()).toBe('[Post!]!')

		const comment = schemaComposer.getOTC('Comment')
		expect(comment.hasField('parent')).toBe(true)
		expect(comment.hasField('replies')).toBe(true)
		expect(comment.getFieldType('parent').toString()).toBe('Comment')
		expect(comment.getFieldType('replies').toString()).toBe('[Comment!]!')
	})

	test('Basic filter input types are properly defined', () => {
		const stringFilterInput = schemaComposer.getITC('StringFilterInput')
		expect(stringFilterInput.hasField('equals')).toBe(true)
		expect(stringFilterInput.hasField('contains')).toBe(true)
		expect(stringFilterInput.hasField('startsWith')).toBe(true)
		expect(stringFilterInput.hasField('endsWith')).toBe(true)

		const dateTimeFilterInput = schemaComposer.getITC('DateTimeFilterInput')
		expect(dateTimeFilterInput.hasField('equals')).toBe(true)
		expect(dateTimeFilterInput.hasField('gt')).toBe(true)
		expect(dateTimeFilterInput.hasField('lt')).toBe(true)

		const numericFilterInput = schemaComposer.getITC('NumericFilterInput')
		expect(numericFilterInput.hasField('equals')).toBe(true)
		expect(numericFilterInput.hasField('gt')).toBe(true)
		expect(numericFilterInput.hasField('lt')).toBe(true)

		const booleanFilterInput = schemaComposer.getITC('BooleanFilterInput')
		expect(booleanFilterInput.hasField('equals')).toBe(true)
		expect(booleanFilterInput.hasField('not')).toBe(true)
	})

	test('Enum types are properly defined', () => {
		expect(schemaComposer.has('ReactionType')).toBe(true)
		expect(schemaComposer.has('UserRole')).toBe(true)
		expect(schemaComposer.has('ContentFormat')).toBe(true)

		const reactionTypeEnum = schemaComposer.getETC('ReactionType')
		const enumValues = reactionTypeEnum.getFields()
		expect(Object.keys(enumValues)).toContain('LIKE')
		expect(Object.keys(enumValues)).toContain('LOVE')
		expect(Object.keys(enumValues)).toContain('HAHA')
	})

	test('Relay compliance', () => {
		expect(schemaComposer.has('Node')).toBe(true)
		const nodeInterface = schemaComposer.getIFTC('Node')
		expect(nodeInterface).toBeTruthy()
		expect(nodeInterface.hasField('id')).toBe(true)
		expect(nodeInterface.getFieldType('id').toString()).toBe('ID!')
	})

	test('Schema is valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})
})
