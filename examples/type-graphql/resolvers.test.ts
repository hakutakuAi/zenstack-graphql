import { expect, test, describe } from 'bun:test'
import 'reflect-metadata'
import { buildSchema } from 'type-graphql'
import { UserResolver, PostResolver } from './src/resolvers'

describe('TypeGraphQL Resolvers Integration', () => {
	test('buildSchema works with generated types and resolvers', async () => {
		const schema = await buildSchema({
			resolvers: [UserResolver, PostResolver],
		})

		expect(schema).toBeDefined()
		expect(schema.getQueryType()).toBeDefined()
		expect(schema.getMutationType()).toBeDefined()
	})

	test('resolvers have correct query methods', () => {
		const userResolver = new UserResolver()
		const postResolver = new PostResolver()

		expect(userResolver.getUsers).toBeInstanceOf(Function)
		expect(userResolver.getUser).toBeInstanceOf(Function)
		expect(userResolver.createUser).toBeInstanceOf(Function)

		expect(postResolver.getPosts).toBeInstanceOf(Function)
		expect(postResolver.getPost).toBeInstanceOf(Function)
		expect(postResolver.createPost).toBeInstanceOf(Function)
		expect(postResolver.publishPost).toBeInstanceOf(Function)
	})

	test('user resolver returns correct data types', () => {
		const userResolver = new UserResolver()

		const users = userResolver.getUsers()
		expect(Array.isArray(users)).toBe(true)
		expect(users.length).toBeGreaterThan(0)

		if (users.length > 0) {
			const user = users[0]
			expect(user).toHaveProperty('id')
			expect(user).toHaveProperty('name')
			expect(user).toHaveProperty('email')
			expect(user).toHaveProperty('createdAt')
			expect(user).toHaveProperty('updatedAt')
		}
	})

	test('post resolver returns correct data types', () => {
		const postResolver = new PostResolver()

		const posts = postResolver.getPosts()
		expect(Array.isArray(posts)).toBe(true)
		expect(posts.length).toBeGreaterThan(0)

		if (posts.length > 0) {
			const post = posts[0]
			expect(post).toHaveProperty('id')
			expect(post).toHaveProperty('title')
			expect(post).toHaveProperty('content')
			expect(post).toHaveProperty('published')
			expect(post).toHaveProperty('authorId')
			expect(post).toHaveProperty('createdAt')
			expect(post).toHaveProperty('updatedAt')
		}
	})

	test('mutations work correctly', () => {
		const userResolver = new UserResolver()
		const postResolver = new PostResolver()

		const newUser = userResolver.createUser('Test User', 'test@example.com', 'Test bio')
		expect(newUser).toHaveProperty('id')
		expect(newUser.name).toBe('Test User')
		expect(newUser.email).toBe('test@example.com')
		expect(newUser.bio).toBe('Test bio')

		const newPost = postResolver.createPost('Test Post', 'Test content', '1')
		expect(newPost).toHaveProperty('id')
		expect(newPost.title).toBe('Test Post')
		expect(newPost.content).toBe('Test content')
		expect(newPost.authorId).toBe('1')
		expect(newPost.published).toBe(false)
	})
})
