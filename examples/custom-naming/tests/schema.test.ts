import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer, ObjectTypeComposer, EnumTypeComposer } from 'graphql-compose'

describe('Custom Naming Example', () => {
	const schemaPath = join(__dirname, '../schema.graphql')
	const schemaComposer = new SchemaComposer()

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaComposer.add(readFileSync(schemaPath, 'utf8'))
	})

	test('Field names use snake_case convention', () => {
		const userAccountTC = schemaComposer.getOTC('UserAccount')

		expect(userAccountTC.hasField('first_name')).toBe(true)
		expect(userAccountTC.hasField('last_name')).toBe(true)
		expect(userAccountTC.hasField('email_address')).toBe(true)
		expect(userAccountTC.hasField('last_login_time')).toBe(true)
		expect(userAccountTC.hasField('profile_settings')).toBe(true)
		expect(userAccountTC.hasField('is_active')).toBe(true)

		expect(userAccountTC.hasField('firstName')).toBe(false)
		expect(userAccountTC.hasField('lastName')).toBe(false)
		expect(userAccountTC.hasField('emailAddress')).toBe(false)

		expect(userAccountTC.getFieldType('first_name').toString()).toBe('String!')
		expect(userAccountTC.getFieldType('email_address').toString()).toBe('String!')
		expect(userAccountTC.getFieldType('is_active').toString()).toBe('Boolean!')
	})

	test('Type names use PascalCase convention', () => {
		expect(schemaComposer.has('UserAccount')).toBe(true)
		expect(schemaComposer.has('BlogPost')).toBe(true)
		expect(schemaComposer.has('Comment')).toBe(true)
	})

	test('Field naming in UserAccount', () => {
		const userAccountTC = schemaComposer.getOTC('UserAccount')

		expect(userAccountTC.hasField('phone_number')).toBe(true)

		expect(userAccountTC.hasField('user_posts')).toBe(true)
		expect(userAccountTC.hasField('userPosts')).toBe(true)
	})

	test('BlogPost model fields and relations', () => {
		const blogPostTC = schemaComposer.getOTC('BlogPost')

		expect(blogPostTC.hasField('title')).toBe(true)
		expect(blogPostTC.hasField('content')).toBe(true)
		expect(blogPostTC.hasField('author')).toBe(true)
		expect(blogPostTC.hasField('author_id')).toBe(true)
		expect(blogPostTC.hasField('publish_date')).toBe(true)
		expect(blogPostTC.hasField('tags')).toBe(true)

		expect(blogPostTC.getFieldType('author').toString()).toBe('UserAccount!')
		expect(blogPostTC.getFieldType('author_id').toString()).toBe('String!')
	})

	test('Enum is not renamed', () => {
		expect(schemaComposer.has('PostStatus')).toBe(true)
		expect(schemaComposer.has('ContentStatus')).toBe(false)

		const postStatusETC = schemaComposer.getETC('PostStatus')
		const enumValues = postStatusETC.getFields()

		expect(Object.keys(enumValues)).toContain('DRAFT')
		expect(Object.keys(enumValues)).toContain('PUBLISHED')
		expect(Object.keys(enumValues)).toContain('ARCHIVED')
	})

	test('Relations follow the naming conventions', () => {
		const userAccountTC = schemaComposer.getOTC('UserAccount')
		const blogPostTC = schemaComposer.getOTC('BlogPost')

		expect(userAccountTC.hasField('user_posts')).toBe(true)
		expect(userAccountTC.hasField('userPosts')).toBe(true)
		expect(userAccountTC.getFieldType('user_posts').toString()).toBe('[BlogPost!]!')

		expect(blogPostTC.hasField('author')).toBe(true)
		expect(blogPostTC.hasField('author_id')).toBe(true)
	})

	test('Connection types follow naming conventions', () => {
		expect(schemaComposer.has('UserAccountConnection')).toBe(true)
		expect(schemaComposer.has('BlogPostConnection')).toBe(true)
		expect(schemaComposer.has('CommentConnection')).toBe(true)

		const userAccountConnectionTC = schemaComposer.getOTC('UserAccountConnection')
		expect(userAccountConnectionTC.hasField('edges')).toBe(true)
		expect(userAccountConnectionTC.hasField('pageInfo')).toBe(true)
		expect(userAccountConnectionTC.hasField('totalCount')).toBe(true)

		const userAccountEdgeTC = schemaComposer.getOTC('UserAccountEdge')
		expect(userAccountEdgeTC.hasField('node')).toBe(true)
		expect(userAccountEdgeTC.hasField('cursor')).toBe(true)
	})

	test('Schema is valid', () => {
		const schema = schemaComposer.buildSchema()
		expect(schema).toBeTruthy()
	})

	test('Issue with custom attribute processing', () => {
		expect(schemaComposer.has('BlogPost')).toBe(true)
		expect(schemaComposer.has('Article')).toBe(false)

		expect(schemaComposer.has('PostStatus')).toBe(true)
		expect(schemaComposer.has('ContentStatus')).toBe(false)

		const commentTC = schemaComposer.getOTC('Comment')
		expect(commentTC.getDescription()).toBe('')

		const contentField = commentTC.getField('content')
		expect(contentField.description).toBeUndefined()
	})
})
